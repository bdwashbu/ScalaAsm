package com.scalaAsm
package assembler

import com.scalaAsm.asm.Tokens._
import com.scalaAsm.utils.Endian
import com.scalaAsm.x86.Instructions.Standard.CALL
import com.scalaAsm.x86.Operands.Constant32
import com.scalaAsm.x86.Operands.Constant8
import com.scalaAsm.linker.Assembled
import com.scalaAsm.asm.Registers
import com.scalaAsm.x86.InstructionResult
import com.scalaAsm.x86.Instructions.Standard
import com.scalaAsm.x86.Instructions.Formats
import com.scalaAsm.x86.Prefixes
import com.scalaAsm.x86.Instructions.`package`.Op

class AsmCompiler(code: Seq[Any], data: Seq[Token]) extends Assembled(code, data) with Standard.Catalog with Formats with Registers with Prefixes
{
  import scala.language.postfixOps
  
  val (rawData, variables) = compileData(data)
    
  val compiledAsm = compileAssembly(this, variables)
  
  val unboundSymbols = compiledAsm.onePass.collect { case ImportRef(name) => name}
    
  val compiledImports = compileImports(rawData.size, unboundSymbols)   
  
  def compileData(dataTokens: Seq[Token]): (Array[Byte], (Int) => Map[String, Int]) = {

    val dataSection: Seq[PostToken] = {
      var parserPosition = 0
      for (token <- dataTokens) yield {
        val result = token match {
	      case Variable(name, value) => PostVar(name, value, parserPosition)
	      case Align(to, filler, _) => ByteOutputPost( Array.fill((to - (parserPosition % to)) % to)(filler))
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
        case PostVar(_,value,_) => Some(value.toCharArray().map(_.toByte))
        case _ => None
    })
      
    val data = Array.fill[Byte](8)(0x00) ++: dataBytes.reduce(_ ++: _)

    // a map of variable to its RVA
    def createDefMap(dataSection: Seq[PostToken]): (Int) => Map[String, Int] = {
    	(dataAddress: Int) => (dataSection flatMap {
	      case PostVar(name, value, pos) => Some((name, pos + dataAddress + 8))
	      case _ => None
	    } toMap)
    }
    
    (data, createDefMap(dataSection))
  }
  
  def compileAssembly(asm: Assembled,
                      variables: (Int) => Map[String, Int]): CompiledAssembly = {

    lazy val varNames    = variables(0).keys.toList
    lazy val procNames   = asm.codeTokens.collect{ case BeginProc(name) => name }
    
    def onePass: Seq[Token] = asm.codeTokens flatMap {
        
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
          case x:LabelRef[_] => parserPosition += 2
          case _ => 
         }
         result
      }
    }
 
    CompiledAssembly(onePass, positionPass)
  }
  
  def finalizeAssembly(variables: Map[String, Int], imports: Map[String, Int], baseOffset: Int): Array[Byte] = {
    
    lazy val varNames = variables.keys.toList
    // Build procedure map
    val procs = compiledAsm.positionPass collect {case Proc(offset, name) => (name, offset)} toMap
    val labels = compiledAsm.positionPass collect {case LabelResolved(offset, name) => (name, offset)} toMap    
    
    val code: Array[Byte] = {
      var parserPosition = 0
      for (token <- compiledAsm.onePass) yield {
        val result = token match {
	        case InstructionToken(inst) => inst.getBytes
	        case Align(to, filler, _) => Array.fill((to - (parserPosition % to)) % to)(filler)
	        case Padding(to, _) => Array.fill(to)(0xCC.toByte)
	        case ProcRef(name) => callNear(Op(*(Constant32(procs(name) - parserPosition - 5)).get.getRelative)).getBytes
	        case InvokeRef(name) => callNear(Op(*(Constant32(imports(name) - parserPosition - 5)).get.getRelative)).getBytes
	        case VarRef(name) => push(Op(Constant32(variables(name) + baseOffset))).getBytes
	        case JmpRefResolved(name) => jmp(*(Constant32(imports(name) + baseOffset))).getBytes
	        case ImportRef(name) => callNear(*(Constant32(imports(name) + baseOffset))).getBytes
	        case LabelRef(name, inst, format) => {
	          val op = (labels(name) - parserPosition - 2).toByte
	          inst(Op(new Constant8(op)), format, Array()).getBytes
	        }
	        case _ => Array[Byte]()
	      }
         token match {
          case sizedToken: SizedToken => parserPosition += sizedToken.size
          case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
          case x:LabelRef[_] => parserPosition += 2
          case _ => 
         }
         result
      }
    }.reduce(_ ++ _)
    

    
    code 
  }
}