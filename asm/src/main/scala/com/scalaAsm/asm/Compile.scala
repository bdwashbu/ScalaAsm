package com.scalaAsm.asm

import com.scalaAsm.asm.Tokens._
import com.scalaAsm.utils.Endian
import com.scalaAsm.x86.Instructions.CALL
import com.scalaAsm.asm.Addressing._
import com.scalaAsm.x86.Instructions.Catalog
import com.scalaAsm.x86.MachineCodeBuilder
import com.scalaAsm.x86.Operands.Constant32
import com.scalaAsm.x86.Operands.Constant8

object AsmCompiler extends Catalog
{
  import scala.language.postfixOps
  
  def compileData(addressOfData: Int, dataTokens: Seq[Token]): (Array[Byte], Map[String, Int]) = {

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
      
    val data = Array.fill[Byte](8)(0x00) ++ dataBytes.reduce(_++_)

    // a map of variable to its RVA
    def createDefMap: Map[String, Int] = {
    	dataSection flatMap {
	      case PostVar(name, value, pos) => Some((name, pos + addressOfData + 8))
	      case _ => None
	    } toMap
    }
    
    (data, createDefMap)
  }
  
  case class CompiledAssembly(onePass: Seq[Token], positionPass: Seq[PostToken])
  
  def compileAssembly(asm: Assembled,
                      variables: Map[String, Int]): CompiledAssembly = {

    lazy val varNames    = variables.keys.toList
    lazy val procNames   = asm.code.collect{ case BeginProc(name) => name }
    
    def onePass: Seq[Token] = asm.code flatMap {
        case x: MachineCodeBuilder => Some(InstructionToken(x.get))
        case x: SizedToken => Some(x)
        case x: DynamicSizedToken => Some(x)
        case proc @ BeginProc(_) => Some(proc)
        case JmpRef(name) => Some(JmpRefResolved(name))
        case Reference(name) if procNames.contains(name) => Some(ProcRef(name))
        case Reference(name) if varNames.contains(name) => Some(VarRef(name))
        case Reference(name) => Some(ImportRef(name))
        case label @ Label(name) => Some(label)
        case labelref @ LabelRef(name,opcode) => Some(labelref)
        case _ => None
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
          case x:LabelRef => parserPosition += 2
          case _ => 
         }
         result
      }
    }
 
    CompiledAssembly(onePass, positionPass)
  }
  
  def finalizeAssembly(asm: CompiledAssembly, variables: Map[String, Int], imports: Map[String, Int], baseOffset: Int): Array[Byte] = {
    
    lazy val varNames = variables.keys.toList
    // Build procedure map
    val procs = asm.positionPass collect {case Proc(offset, name) => (name, offset)} toMap
    val labels = asm.positionPass collect {case LabelResolved(offset, name) => (name, offset)} toMap    
    
    val code: Array[Byte] = {
      var parserPosition = 0
      for (token <- asm.onePass) yield {
        val result = token match {
	        case InstructionToken(inst) => inst.code
	        case Align(to, filler, _) => Array.fill((to - (parserPosition % to)) % to)(filler)
	        case Padding(to, _) => Array.fill(to)(0xCC.toByte)
	        case ProcRef(name) => callNear(*(Constant32(procs(name) - parserPosition - 5)).rel32).get.code
	        case VarRef(name) => push(Constant32(variables(name) + baseOffset)).get.code
	        case JmpRefResolved(name) => jmp(*(Constant32(imports(name) + baseOffset))).get.code
	        case ImportRef(name) => callNear(*(Constant32(imports(name) + baseOffset))).get.code
	        case LabelRef(name, inst) => inst.get(new Constant8((labels(name) - parserPosition - 2).toByte)).get.code
	        case _ => Array[Byte]()
	      }
         token match {
          case sizedToken: SizedToken => parserPosition += sizedToken.size
          case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
          case x:LabelRef => parserPosition += 2
          case _ => 
         }
         result
      }
    }.reduce(_ ++ _)
    

    
    code 
  }
}