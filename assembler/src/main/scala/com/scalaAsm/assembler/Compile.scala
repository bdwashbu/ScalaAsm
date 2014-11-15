package com.scalaAsm
package assembler

import com.scalaAsm.asm.Tokens._
import com.scalaAsm.x86.Instructions.Standard.CALL
import com.scalaAsm.x86.Operands.Constant32
import com.scalaAsm.x86.Operands.Constant8
import com.scalaAsm.coff.Assembled
import com.scalaAsm.asm.Registers
import com.scalaAsm.asm.DataSection
import com.scalaAsm.x86.InstructionResult
import com.scalaAsm.x86.Instructions.Standard
import com.scalaAsm.x86.Instructions.Formats
import com.scalaAsm.x86.Instructions.Catalog
import com.scalaAsm.x86.Instructions.OneMachineCode
import com.scalaAsm.x86.Operands.addr
import com.scalaAsm.x86.Instructions.TwoMachineCode
import com.scalaAsm.x86.Operands.Op
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
import com.scalaAsm.coff.{IMAGE_SYM_CLASS_EXTERNAL, IMAGE_SYM_DTYPE_FUNCTION}

class Assembler extends Catalog.Standard with Formats with Addressing {
  self =>
  import scala.language.postfixOps

  def assemble[Mode <: x86Mode](program: AsmProgram[Mode]): Coff = {

    val codeTokens: ListBuffer[Any] = program.sections.collect{ case (x: AsmProgram[_]#CodeSection) => x}.flatMap { seg => seg.build(seg.builder.toSeq) }

    val dataTokens = program.sections.collect{ case (x: DataSection) => x} flatMap { seg => seg.compile }

    val (rawData, variablesSymbols) = compileData(dataTokens)

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
    
    val code: Array[Byte] = {
        
        var parserPosition = 0
        
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
          Characteristic.READ.id) 
      , code.toArray)
   
    val dataSection = Section(
      SectionHeader(
        name = "data",
        virtualSize = rawData.length,
        virtualAddress = 0x2000,
        sizeOfRawData = 0x200,
        pointerToRawData = 0x600,
        relocPtr = 0,
        linenumPtr = 0,
        relocations = 0,
        lineNumbers = 0,
        characteristics = Characteristic.INITIALIZED.id |
          Characteristic.READ.id |
          Characteristic.WRITE.id)
      , rawData)
      
      val symbols = (compiledAsm.positionPass collect {
        case Proc(offset, name) => CoffSymbol(name, offset, 2, IMAGE_SYM_DTYPE_FUNCTION, IMAGE_SYM_CLASS_EXTERNAL);
        case LabelResolved(offset, name) => CoffSymbol(name, offset, 2, IMAGE_SYM_DTYPE_FUNCTION, IMAGE_SYM_CLASS_EXTERNAL)
        case InvokeRef(offset,name) => CoffSymbol(name, offset, 0, IMAGE_SYM_DTYPE_FUNCTION, IMAGE_SYM_CLASS_EXTERNAL)
        case ImportRef(offset,name) => CoffSymbol(name, offset, 0, IMAGE_SYM_DTYPE_FUNCTION, IMAGE_SYM_CLASS_EXTERNAL)
      }) ++ variablesSymbols
      
      def getRelocations: Seq[Relocation] = {
        var parserPosition = 0

        compiledAsm.onePass.flatMap { token =>
          var result: Option[Relocation] = None
          token match {
            case InstructionToken(inst) => inst match {
                case OneMachineCode(addr(name), _) =>
                  result = Some(Relocation(parserPosition+2, symbols.find { sym => sym.name == name }.get, 1))
                case TwoMachineCode(addr(name), _, _) =>
                  result = Some(Relocation(parserPosition+2, symbols.find { sym => sym.name == name }.get, 1))
                case TwoMachineCode(_, addr(name), _) =>
                  result = Some(Relocation(parserPosition+2, symbols.find { sym => sym.name == name }.get, 1))
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
            case sizedToken: SizedToken => parserPosition += sizedToken.size
            case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
            case x: LabelRef => parserPosition += 2
            case _ =>
          }
          result
        }
      }
    
    Coff(
      symbols = symbols,
      relocations = getRelocations,
      sections = Seq(codeSection, dataSection)
    )
  }

  def compileData(dataTokens: Seq[Token]): (Array[Byte], Seq[CoffSymbol]) = {

    // Here, we implicitly add a "KEEP" variable to hold results
    
    val dataSection: Seq[PostToken] = {
      var parserPosition = 0
      for (token <- Variable("KEEP", "\0\0\0\0") +: dataTokens) yield {
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

    val data = dataSection.map {
      case ByteOutputPost(padding) => padding
      case PostVar(_, value, _) => value.toCharArray().map(_.toByte)
      case _ => Array[Byte]()
    }.reduce(_ ++ _)

    // a map of variable to its RVA
    def createDefMap(dataSection: Seq[PostToken]): Seq[CoffSymbol] = {
        dataSection flatMap {
          case PostVar(name, value, pos) => Some(CoffSymbol(name, pos, 1, IMAGE_SYM_DTYPE_FUNCTION, IMAGE_SYM_CLASS_EXTERNAL)) // need the +8?
          case _ => None
        }
    }

    (data, createDefMap(dataSection))
  }

  case class CompiledAssembly(onePass: Seq[Token], positionPass: Seq[PostToken])

}