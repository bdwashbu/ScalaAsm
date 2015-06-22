package com.scalaAsm
package assembler

import com.scalaAsm.asm._
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.x86.Instructions.General._
import com.scalaAsm.x86.Instructions._
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.asm.DataSection
import com.scalaAsm.x86.InstructionResult

import com.scalaAsm.x86.Instructions.OneMachineCode
import com.scalaAsm.x86.Instructions.TwoMachineCode
import scala.collection.mutable.ListBuffer
import com.scalaAsm.coff.Relocation
import scala.collection.mutable.ArrayBuffer
import com.scalaAsm.coff.CoffSymbol
import com.scalaAsm.coff.Section
import com.scalaAsm.coff.Coff
import com.scalaAsm.coff.SectionHeader
import com.scalaAsm.coff.Characteristic
import com.scalaAsm.coff.{ IMAGE_SYM_CLASS_EXTERNAL, IMAGE_SYM_DTYPE_FUNCTION }

import com.scalaAsm.x86.InstructionResult

class Assembler {
  self =>
  import scala.language.postfixOps
  import com.scalaAsm.x86.Instructions.General._
  
  case class CompiledAssembly(onePass: Seq[Token], positionPass: Seq[PostToken], variablesSymbols: Seq[CoffSymbol],
      rawData: Array[Byte], prettyPass: Seq[InstructionResult])
  
  
      
  def preassemble(program: AsmProgram): CompiledAssembly = {
    
    lazy val procTokens = program.sections.collect { case (x: AsmProgram#CodeSection) => x }.flatMap { seg => seg.builder.toSeq }
    lazy val procNames = procTokens.collect { case ProcedureToken(name,_) => name }

    def compileAssembly: CompiledAssembly = {
      
      def resolveFunctionRef: Seq[InstructionResult] = program.codeTokens map {
        case FunctionReference(refName) =>
          if (procNames.contains(refName)) {
            ProcRef(refName)
          } else {
            ImportRef(0, refName)
          }
        case x => x
      }

      def onePass: Seq[Token] = resolveFunctionRef flatMap {

        case x: SizedToken                                   => Some(x)
        case x: DynamicSizedToken                            => Some(x)
        case proc @ BeginProc(_)                             => Some(proc) 
        case Invoke(name)                                    => Some(InvokeRef(0, name))
        case Reference(name) if program.variableNames.contains(name) => Some(VarRef(name))
        case label @ Label(name)                             => Some(label)
        case labelref @ LabelRef(name, inst)                 => Some(labelref)
        case x: InstructionResult                            => Some(InstructionToken(x))
      }

      def positionPass: Seq[PostToken] = {
        var parserPosition = 0
        onePass flatMap { token =>
          val result = token match {
            case BeginProc(name)    => Some(Proc(parserPosition, name))
            case Label(name)        => Some(LabelResolved(parserPosition, name))
            case InvokeRef(_, name) => Some(InvokeRef(parserPosition, name))
            case ImportRef(_, name) => Some(ImportRef(parserPosition, name))
            case _                  => None
          }
          token match {
            case sizedToken: SizedToken        => parserPosition += sizedToken.size
            case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
            case x: LabelRef                   => parserPosition += 2
            case _                             =>
          }
          result
        }
      }
      
      val prettyPass: Seq[InstructionResult] = onePass flatMap { token =>
        token match {
          case InstructionToken(inst) => Some(inst)
          case ProcRef(_) | InvokeRef(_, _) | ImportRef(_, _) => Some(asm"call 0".head)
          case VarRef(_) => Some(asm"push 0".head)
          case LabelRef(_, inst) => Some(inst(new Constant(0.toByte) {}, Seq()))
          case _ => None
        }
      }

     val (rawData, variablesSymbols) = compileData(program.varTokens)
      
      CompiledAssembly(onePass, positionPass, variablesSymbols, rawData, prettyPass)
    }

    compileAssembly
  }
  
  def assemble(program: AsmProgram): Coff = {

    val compiledAsm = preassemble(program)

    val code: Array[Byte] = {

      var parserPosition = 0

      val code = ArrayBuffer[Byte]()

      for (token <- compiledAsm.onePass) {
        val result = token match {
          case InstructionToken(inst) => inst()
          case Padding(to, _) => Array.fill(to)(0xCC.toByte)
          case ProcRef(_) | InvokeRef(_, _) | ImportRef(_, _) => asm"call 0".head.apply
          case VarRef(_) => asm"push 0".head.apply
          case LabelRef(_, inst) => inst(new Constant(0.toByte) {}, Seq()).apply
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
          case sizedToken: SizedToken        => parserPosition += sizedToken.size
          case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
          case x: LabelRef                   => parserPosition += 2
          case _                             =>
        }
      }

      code.toArray
    }

    val codeSection = Section(
      SectionHeader(
        name = "code",
        virtualSize = code.length,
        virtualAddress = 0x1000,
        sizeOfRawData = 0x200,
        pointerToRawData = 0x400,
        relocPtr = 0,
        linenumPtr = 0,
        relocations = 0,
        lineNumbers = 0,
        characteristics = Characteristic.CODE.id |
          Characteristic.EXECUTE.id |
          Characteristic.READ.id), code.toArray)

    val dataSection = Section(
      SectionHeader(
        name = "data",
        virtualSize = compiledAsm.rawData.length,
        virtualAddress = 0x2000,
        sizeOfRawData = 0x200,
        pointerToRawData = 0x600,
        relocPtr = 0,
        linenumPtr = 0,
        relocations = 0,
        lineNumbers = 0,
        characteristics = Characteristic.INITIALIZED.id |
          Characteristic.READ.id |
          Characteristic.WRITE.id), compiledAsm.rawData)

    val symbols = (compiledAsm.positionPass collect {
      case Proc(offset, name)          => CoffSymbol(name, offset, 2, IMAGE_SYM_DTYPE_FUNCTION(0), IMAGE_SYM_CLASS_EXTERNAL, Nil)
      case LabelResolved(offset, name) => CoffSymbol(name, offset, 2, IMAGE_SYM_DTYPE_FUNCTION(0), IMAGE_SYM_CLASS_EXTERNAL, Nil)
      case InvokeRef(offset, name)     => CoffSymbol(name, offset, 0, IMAGE_SYM_DTYPE_FUNCTION(0), IMAGE_SYM_CLASS_EXTERNAL, Nil)
      case ImportRef(offset, name)     => CoffSymbol(name, offset, 0, IMAGE_SYM_DTYPE_FUNCTION(0), IMAGE_SYM_CLASS_EXTERNAL, Nil)
    }) ++ compiledAsm.variablesSymbols

    def getRelocations: Seq[Relocation] = {
      var parserPosition = 0
      
      val symIndex = symbols.zipWithIndex

      compiledAsm.onePass.flatMap { token =>
        var result: Option[Relocation] = None
        token match {
          case InstructionToken(inst) => inst match {
            case OneMachineCode(_, AbsoluteAddress(address, name), _, _, _) =>
              result = Some(Relocation(parserPosition + 2, symIndex.find { sym => sym._1.name == name }.get._2, 1))
            case TwoMachineCode(_, AbsoluteAddress(address, name), _, _, _, _) =>
              result = Some(Relocation(parserPosition + 2, symIndex.find { sym => sym._1.name == name }.get._2, 1))
            case TwoMachineCode(_, _, AbsoluteAddress(address, name), _, _, _) =>
              result = Some(Relocation(parserPosition + 2, symIndex.find { sym => sym._1.name == name }.get._2, 1))
            case _ =>
          }
          case ProcRef(name) =>
            result = Some(Relocation(parserPosition, symIndex.find { sym => sym._1.name == name }.get._2, 2))
          case InvokeRef(_, name) =>
            result = Some(Relocation(parserPosition + 1, symIndex.find { sym => sym._1.name == name }.get._2, 20))
          case VarRef(name) =>
            result = Some(Relocation(parserPosition + 1, symIndex.find { sym => sym._1.name == name }.get._2, 6))
          case ImportRef(_, name) =>
            result = Some(Relocation(parserPosition + 1, symIndex.find { sym => sym._1.name == name }.get._2, 20))
          case LabelRef(name, inst) =>
            result = Some(Relocation(parserPosition, symIndex.find { sym => sym._1.name == name }.get._2, 7))
          case _ => None
        }
        token match {
          case sizedToken: SizedToken        => parserPosition += sizedToken.size
          case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
          case x: LabelRef                   => parserPosition += 2
          case _                             =>
        }
        result
      }
    }

    Coff(
      sections = Seq(codeSection, dataSection),
      relocations = getRelocations,
      symbols = symbols)
  }

  private def compileData(dataTokens: Seq[Token]): (Array[Byte], Seq[CoffSymbol]) = {

    // Here, we implicitly add a "KEEP" variable to hold results

    val dataSection: Seq[PostToken] = {
      var parserPosition: Int = 0
      for (token <- Variable("KEEP", "\u0000\u0000\u0000\u0000") +: dataTokens) yield {
        val result = token match {
          case Variable(name, value) => PostVar(name, value, parserPosition)
          case Align(to, filler, _)  => ByteOutputPost(Array.fill((to - (parserPosition % to)) % to)(filler))
        }
        token match {
          case sizedToken: SizedToken        => parserPosition += sizedToken.size
          case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
        }

        result
      }
    }

    val data = dataSection.map {
      case ByteOutputPost(padding) => padding
      case PostVar(_, value, _)    => value.toCharArray().map(_.toByte)
      case _                       => Array[Byte]()
    }.reduce(_ ++ _)

    // a map of variable to its RVA
    def createDefMap(dataSection: Seq[PostToken]): Seq[CoffSymbol] = {
      dataSection flatMap {
        case PostVar(name, value, pos) => Some(CoffSymbol(name, pos, 1, IMAGE_SYM_DTYPE_FUNCTION(0), IMAGE_SYM_CLASS_EXTERNAL, Nil))
        case _                         => None
      }
    }

    (data, createDefMap(dataSection))
  }
}