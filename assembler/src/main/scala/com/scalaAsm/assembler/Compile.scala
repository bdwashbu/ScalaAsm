package com.scalaAsm
package assembler

import com.scalaAsm.asm.Tokens._
//import com.scalaAsm.x86.Instructions.General._
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.asm.Registers
import com.scalaAsm.asm.DataSection
import com.scalaAsm.x86.InstructionResult
//import com.scalaAsm.x86.Instructions.Standard
import com.scalaAsm.x86.Instructions.Formats
import com.scalaAsm.x86.Instructions.Catalog
import com.scalaAsm.x86.Instructions.OneMachineCode
import com.scalaAsm.x86.Instructions.TwoMachineCode
import com.scalaAsm.asm.AsmProgram
import scala.collection.mutable.ListBuffer
import com.scalaAsm.asm.x86Mode
import com.scalaAsm.asm.Addressing
import com.scalaAsm.coff.Relocation
import scala.collection.mutable.ArrayBuffer
import com.scalaAsm.coff.CoffSymbol
import com.scalaAsm.coff.Section
import com.scalaAsm.coff.Coff
import com.scalaAsm.coff.SectionHeader
import com.scalaAsm.coff.Characteristic
import com.scalaAsm.coff.{ IMAGE_SYM_CLASS_EXTERNAL, IMAGE_SYM_DTYPE_FUNCTION }

class Assembler extends Catalog.Standard with Formats with Addressing {
  self =>
  import scala.language.postfixOps
  
  case class CompiledAssembly(onePass: Seq[Token], positionPass: Seq[PostToken], variablesSymbols: Seq[CoffSymbol],
      rawData: Array[Byte], prettyPass: Seq[InstructionResult])
  
  def preassemble[Mode <: x86Mode](program: AsmProgram[Mode]): CompiledAssembly = {

    

    def compileAssembly: CompiledAssembly = {

      def onePass: Seq[Token] = program.codeTokens flatMap {

        case x: SizedToken                                   => Some(x)
        case x: DynamicSizedToken                            => Some(x)
        case proc @ BeginProc(_)                             => Some(proc) 
        case Invoke(name)                                    => Some(InvokeRef(0, name))
        case Reference(name) if program.variableNames.contains(name) => Some(VarRef(name))
        case label @ Label(name)                             => Some(label)
        case labelref @ LabelRef(name, inst, format)         => Some(labelref)
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
          case ProcRef(_) | InvokeRef(_, _) | ImportRef(_, _) => Some(callNear(program.Op(Constant32(0))).apply)
          case VarRef(_) => Some(push(program.Op(Constant32(0))).apply)
          case LabelRef(_, inst, format) => Some(inst(program.Op(new Constant8(0)), format, Seq()))
          case _ => None
        }
      }

     val (rawData, variablesSymbols) = compileData(program.varTokens)
      
      CompiledAssembly(onePass, positionPass, variablesSymbols, rawData, prettyPass)
    }

    compileAssembly
  }
  
  def assemble[Mode <: x86Mode](program: AsmProgram[Mode]): Coff = {

    val compiledAsm = preassemble(program)

    val code: Array[Byte] = {

      var parserPosition = 0

      val code = ArrayBuffer[Byte]()

      for (token <- compiledAsm.onePass) {
        val result = token match {
          case InstructionToken(inst) => inst()
          case Padding(to, _) => Array.fill(to)(0xCC.toByte)
          case ProcRef(_) | InvokeRef(_, _) | ImportRef(_, _) => callNear(program.Op(Constant32(0))).apply.apply
          case VarRef(_) => push(program.Op(Constant32(0))).apply.apply
          case LabelRef(_, inst, format) => inst(program.Op(new Constant8(0)), format, Seq()).apply
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

      compiledAsm.onePass.flatMap { token =>
        var result: Option[Relocation] = None
        token match {
          case InstructionToken(inst) => inst match {
            case OneMachineCode(_, op1, _, _, _, _) if op1().isInstanceOf[AbsoluteAddress[_]] =>
              val name = op1().asInstanceOf[AbsoluteAddress[_]].name.get
              result = Some(Relocation(parserPosition + 2, symbols.find { sym => sym.name == name }.get, 1))
            case TwoMachineCode(_, op1, _, _, _, _, _) if op1().isInstanceOf[AbsoluteAddress[_]] =>
              val name = op1().asInstanceOf[AbsoluteAddress[_]].name.get
              result = Some(Relocation(parserPosition + 2, symbols.find { sym => sym.name == name }.get, 1))
            case TwoMachineCode(_, _, op2, _, _, _, _) if op2().isInstanceOf[AbsoluteAddress[_]] =>
              val name = op2().asInstanceOf[AbsoluteAddress[_]].name.get
              result = Some(Relocation(parserPosition + 2, symbols.find { sym => sym.name == name }.get, 1))
            case _ =>
          }
          case ProcRef(name) =>
            result = Some(Relocation(parserPosition, symbols.find { sym => sym.name == name }.get, 2))
          case InvokeRef(_, name) =>
            result = Some(Relocation(parserPosition + 1, symbols.find { sym => sym.name == name }.get, 20))
          case VarRef(name) =>
            result = Some(Relocation(parserPosition + 1, symbols.find { sym => sym.name == name }.get, 6))
          case ImportRef(_, name) =>
            result = Some(Relocation(parserPosition + 1, symbols.find { sym => sym.name == name }.get, 20))
          case LabelRef(name, inst, format) =>
            result = Some(Relocation(parserPosition, symbols.find { sym => sym.name == name }.get, 7))
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
      var parserPosition = 0
      for (token <- Variable("KEEP", "\0\0\0\0") +: dataTokens) yield {
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