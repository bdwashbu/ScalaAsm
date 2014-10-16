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
import com.scalaAsm.asm.x86Mode
import com.scalaAsm.asm.Addressing
import com.scalaAsm.coff.Relocation
import scala.collection.mutable.ArrayBuffer
import com.scalaAsm.coff.CoffSymbol

class Assembler extends Standard.Catalog with Formats with Addressing {
  self =>
  import scala.language.postfixOps

  def assemble[Mode <: x86Mode](program: AsmProgram[Mode]): Assembled = {

    val codeTokens: ListBuffer[Any] = program.codeSections.flatMap { seg => seg.build(seg.builder.toSeq) }

    val dataTokens = program.dataSections flatMap { seg => seg.compile }

    val (rawData2, variablesSymbols) = compileData(dataTokens)

    def compileAssembly(variableNames: Seq[String]): CompiledAssembly = {

      lazy val procNames = codeTokens.collect { case BeginProc(name) => name }

      def onePass: Seq[Token] = codeTokens flatMap {

        case x: SizedToken => Some(x)
        case x: DynamicSizedToken => Some(x)
        case proc @ BeginProc(_) => Some(proc)
        case JmpRef(name) => Some(JmpRefResolved(name))
        case Invoke(name) => Some(InvokeRef(0, name))
        case Reference(name) if procNames.contains(name) => Some(ProcRef(name))
        case Reference(name) if variableNames.contains(name) => Some(VarRef(name))
        case Reference(name) => Some(ImportRef(0, name))
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
            case InvokeRef(_,name) => Some(InvokeRef(parserPosition, name))
            case ImportRef(_,name) => Some(ImportRef(parserPosition, name))
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
    
    val compiledAsm = compileAssembly(variablesSymbols.map(_.name).toSeq)

    new Assembled {
      val rawData = rawData2
      val symbols = compiledAsm.onePass.collect { case ImportRef(offset, name) => CoffSymbol(name,offset); case InvokeRef(offset, name) => CoffSymbol(name,offset)} ++ variablesSymbols
      val relocations = ListBuffer[Relocation]()
      //val varMap: Map[String, Int] = variablesSymbols map { case CoffSymbol(name, offset) => (name, offset) } toMap // apply offset

      // Build procedure map
      val refSymbols: Map[String, Int] = (compiledAsm.positionPass collect { case Proc(offset, name) => (name, offset); case LabelResolved(offset, name) => (name, offset) } toMap) ++ (variablesSymbols map { case CoffSymbol(name, offset) => (name, offset) } toMap)
      
      def getCode(addressOfData: Int, baseOffset: Int): ArrayBuffer[Byte] = {
        
        var parserPosition = 0

        for (token <- compiledAsm.onePass) {
          token match {
            case InstructionToken(inst) => inst match {
                case OneMachineCodeBuilder(addr(name)) =>
                  relocations += Relocation(parserPosition+2, name, 1)
                case TwoMachineCodeBuilder(addr(name), _) =>
                  relocations += Relocation(parserPosition+2, name, 1)
                case TwoMachineCodeBuilder(_, addr(name)) =>
                  relocations += Relocation(parserPosition+2, name, 1)
                case _ =>
              }
            case ProcRef(name) =>
              relocations += Relocation(parserPosition, name, 2)
            case InvokeRef(_, name) =>
              relocations += Relocation(parserPosition, name, 3)
            case VarRef(name) =>
              relocations += Relocation(parserPosition, name, 4)
            case ImportRef(_, name) =>
              relocations += Relocation(parserPosition, name, 3)
            case LabelRef(name, inst, format) =>
              relocations += Relocation(parserPosition, name, 6)
            case _ =>
          }
          token match {
            case sizedToken: SizedToken => parserPosition += sizedToken.size
            case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
            case x: LabelRef => parserPosition += 2
            case _ =>
          }
        }
        
        val code = ArrayBuffer[Byte]()
        
        for (token <- compiledAsm.onePass) {
          val result = token match {
            case InstructionToken(inst) => inst.getBytes
            case Padding(to, _) => Array.fill(to)(0xCC.toByte)
            case ProcRef(_) | InvokeRef(_,_) | ImportRef(_,_) => callNear(*(Constant32(0)).get.getRelative).getBytes
            case VarRef(_) => push(Op(Constant32(0))).getBytes
            case LabelRef(_, inst, format) => inst(Op(new Constant8(0)), format, Seq()).getBytes
            case _ => Array[Byte]()
          }
          code ++= result
        }

        parserPosition = 0
        for (token <- compiledAsm.onePass) {
          token match {
            case Align(to, filler, _) => {
              for (i <- 0 until (to - (parserPosition % to)) % to) {
                code.insert(parserPosition, filler)
              }
            }
            case _ =>
          }
          token match {
            case sizedToken: SizedToken => parserPosition += sizedToken.size
            case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
            case x: LabelRef => parserPosition += 2
            case _ =>
          }
        }
        
        code
      }
    }
  }

  //val compiledImports = compileImports(rawData.size, unboundSymbols)   

  def compileData(dataTokens: Seq[Token]): (Array[Byte], Seq[CoffSymbol]) = {

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

    val dataBytes = dataSection flatMap {
      case ByteOutputPost(padding) => Some(padding)
      case PostVar(_, value, _) => Some(value.toCharArray().map(_.toByte))
      case _ => None
    }

    val data = Array.fill[Byte](8)(0x00) ++: dataBytes.reduce(_ ++: _)

    // a map of variable to its RVA
    def createDefMap(dataSection: Seq[PostToken]): Seq[CoffSymbol] = {
        dataSection flatMap {
          case PostVar(name, value, pos) => Some(CoffSymbol(name, (pos + 8).toShort)) // need the +8?
          case _ => None
        }
    }

    (data, createDefMap(dataSection))
  }

  case class CompiledAssembly(onePass: Seq[Token], positionPass: Seq[PostToken])

}