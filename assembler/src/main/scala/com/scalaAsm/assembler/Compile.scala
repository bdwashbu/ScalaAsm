package com.scalaAsm
package assembler

import com.scalaAsm.asm.Tokens._
import com.scalaAsm.x86.Instructions.Standard.CALL_1
import com.scalaAsm.x86.Operands.Constant32
import com.scalaAsm.x86.Operands.Constant8
import com.scalaAsm.coff.Assembled
import com.scalaAsm.asm.Registers
import com.scalaAsm.x86.InstructionResult
import com.scalaAsm.x86.Instructions.Standard
import com.scalaAsm.x86.Instructions.Formats
import com.scalaAsm.x86.Instructions.OneMachineCodeBuilder
import com.scalaAsm.x86.Operands.addr
import com.scalaAsm.x86.Instructions.TwoMachineCodeBuilder
import com.scalaAsm.x86.Operands.Op
import com.scalaAsm.asm.AsmProgram
import scala.collection.mutable.ListBuffer

class Assembler extends Standard.Catalog with Formats with Registers {
  self =>
  import scala.language.postfixOps

  def assemble(program: AsmProgram): Assembled = {

    val codeTokens: ListBuffer[Any] = program.codeSections.flatMap { seg => seg.build(seg.builder.toSeq) }

    val dataTokens = program.dataSections flatMap { seg => seg.compile }

    val (rawData2, variables2) = compileData(dataTokens)

    def compileAssembly(variables: (Int) => Map[String, Int]): CompiledAssembly = {

      lazy val varNames = variables(0).keys.toList
      lazy val procNames = codeTokens.collect { case BeginProc(name) => name }

      def onePass: Seq[Token] = codeTokens flatMap {

        case x: SizedToken => Some(x)
        case x: DynamicSizedToken => Some(x)
        case proc @ BeginProc(_) => Some(proc)
        case JmpRef(name) => Some(JmpRefResolved(name))
        case Invoke(name) => Some(InvokeRef(name))
        case Reference(name) if procNames.contains(name) => Some(ProcRef(name))
        case Reference(name) if varNames.contains(name) => Some(VarRef(name))
        case Reference(name) => Some(ImportRef(name))
        case label @ Label(name) => Some(label)
        case labelref @ LabelRef(name, inst, format) => Some(labelref)
        case x: InstructionResult => Some(InstructionToken(x))
      }

      def positionPass: Seq[PostToken] = {
        var parserPosition = 0
        onePass flatMap { token =>
          val result = token match {
            case BeginProc(name) => Some(Proc(parserPosition, name))
            case Label(name) => Some(LabelResolved(parserPosition, name))
            case _ => None
          }
          token match {
            case sizedToken: SizedToken => parserPosition += sizedToken.size
            case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
            case x: LabelRef => parserPosition += 2
            case _ =>
          }
          result
        }
      }

      CompiledAssembly(onePass, positionPass)
    }
    
    val compiledAsm2 = compileAssembly(variables2)

    new Assembled {
      val rawData = rawData2
      val variables = variables2
      val compiledAsm = compiledAsm2
      val unboundSymbols = Set(compiledAsm.onePass.collect { case ImportRef(name) => name } ++
        compiledAsm.onePass.collect { case InvokeRef(name) => name }).flatten.toSeq
      
      def finalizeAssembly(addressOfData: Int, imports: Map[String, Int], imports64: Map[String, Int], baseOffset: Int): Array[Byte] = {
        val varMap = variables(addressOfData)
        lazy val varNames = varMap.keys.toList
        // Build procedure map
        val procs = compiledAsm.positionPass collect { case Proc(offset, name) => (name, offset) } toMap
        val labels = compiledAsm.positionPass collect { case LabelResolved(offset, name) => (name, offset) } toMap

        //    for (token <- compiledAsm.onePass) {
        //      token match {
        //        case InstructionToken(inst) => inst match {
        //          case OneMachineCodeBuilder(x: addr) => x.variables = variables; x.baseOffset = baseOffset
        //          case TwoMachineCodeBuilder(op1: addr, _) => op1.variables = variables; op1.baseOffset = baseOffset
        //          case TwoMachineCodeBuilder(_, op2: addr) => op2.variables = variables; op2.baseOffset = baseOffset
        //          case _ =>
        //        }
        //        case _ =>
        //      }
        //    }

        val code: Array[Byte] = {
          var parserPosition = 0
          for (token <- compiledAsm.onePass) yield {
            val result = token match {
              case InstructionToken(inst) => {
                inst match {
                  case OneMachineCodeBuilder(x: addr) =>
                    x.variables = varMap; x.baseOffset = baseOffset; x.parserPosition = parserPosition
                  case TwoMachineCodeBuilder(op1: addr, _) =>
                    op1.variables = varMap; op1.baseOffset = baseOffset; op1.parserPosition = parserPosition
                  case TwoMachineCodeBuilder(_, op2: addr) =>
                    op2.variables = varMap; op2.baseOffset = baseOffset; op2.parserPosition = parserPosition
                  case _ =>
                }
                inst.getBytes
              }
              case Align(to, filler, _) => Array.fill((to - (parserPosition % to)) % to)(filler)
              case Padding(to, _) => Array.fill(to)(0xCC.toByte)
              case ProcRef(name) => callNear(*(Constant32(procs(name) - parserPosition - 5)).get.getRelative).getBytes
              case InvokeRef(name) => callNear(*(Constant32(imports64(name) - (parserPosition + 0x1000) - 5)).get.getRelative).getBytes //callNear(*(Constant32(imports(name) - parserPosition - 5)).get.getRelative).getBytes
              case VarRef(name) => push(Op(Constant32(varMap(name) + baseOffset - 0x1000))).getBytes // fix
              case JmpRefResolved(name) => jmp(*(Constant32(imports(name) + baseOffset))).getBytes
              case ImportRef(name) => callNear(*(Constant32(imports64(name) - (parserPosition + 0x1000) - 5)).get.getRelative).getBytes
              case LabelRef(name, inst, format) => {
                val op = (labels(name) - parserPosition - 2).toByte
                inst(Op(new Constant8(op)), format, Seq()).getBytes
              }
              case _ => Array[Byte]()
            }
            token match {
              case sizedToken: SizedToken => parserPosition += sizedToken.size
              case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
              case x: LabelRef => parserPosition += 2
              case _ =>
            }
            result
          }
        }.reduce(_ ++ _)

        code
      }
    }
  }

  //val compiledImports = compileImports(rawData.size, unboundSymbols)   

  def compileData(dataTokens: Seq[Token]): (Array[Byte], (Int) => Map[String, Int]) = {

    val dataSection: Seq[PostToken] = {
      var parserPosition = 0
      for (token <- dataTokens) yield {
        val result = token match {
          case Variable(name, value) => PostVar(name, value, parserPosition)
          case Align(to, filler, _) => ByteOutputPost(Array.fill((to - (parserPosition % to)) % to)(filler))
        }
        token match {
          case sizedToken: SizedToken => parserPosition += sizedToken.size
          case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
        }

        result
      }
    }

    val dataBytes = (dataSection flatMap {
      case ByteOutputPost(padding) => Some(padding)
      case PostVar(_, value, _) => Some(value.toCharArray().map(_.toByte))
      case _ => None
    })

    val data = Array.fill[Byte](8)(0x00) ++: dataBytes.reduce(_ ++: _)

    // a map of variable to its RVA
    def createDefMap(dataSection: Seq[PostToken]): (Int) => Map[String, Int] = {
      (dataAddress: Int) =>
        (dataSection flatMap {
          case PostVar(name, value, pos) => Some((name, pos + dataAddress + 8))
          case _ => None
        } toMap)
    }

    (data, createDefMap(dataSection))
  }

  case class CompiledAssembly(onePass: Seq[Token], positionPass: Seq[PostToken])

}