package com.scalaAsm.asm

import com.scalaAsm.asm.Tokens._
import com.scalaAsm.utils.Endian
import com.scalaAsm.x86.Instructions.CALL
import com.scalaAsm.x86.Operands.{Immediate16, Immediate32, Immediate8}
import com.scalaAsm.asm.Addressing._
import com.scalaAsm.x86.Instructions.Catalog


object AsmCompiler extends Catalog
{
  def compileData(addressOfData: Int, dataTokens: Seq[Token]): (Array[Byte], Map[String, Int]) = {

    val dataSection: Seq[PostToken] = {
      var parserPosition = 0
      for (token <- dataTokens) yield {
        val result = token match {
	      case Variable(name, value, _) => PostVar(name, value, parserPosition)
	      case Align(to, filler, _) => ByteOutputPost( Array.fill((to - (parserPosition % to)) % to)(filler))
        }
        token match {
          case sizedToken: SizedToken => parserPosition += sizedToken.size
          case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
        }
        
        result
      }      
    }
    
    val dataBytes = (dataSection.flatMap {
        case ByteOutputPost(padding) => Some(padding)
        case PostVar(_,value,_) => Some(value.toCharArray().map(_.toByte))
        case _ => None
    })
      
    val data = Array.fill[Byte](8)(0x00) ++ dataBytes.reduce(_++_)

    // a map of variable to its RVA
    def createDefMap: Map[String, Int] = {
    	dataSection.flatMap {
	      case PostVar(name, value, pos) => Some((name, pos + addressOfData + 8))
	      case _ => None
	    }.toMap
    }
    
    (data, createDefMap)
  }
  
  def compileAssembly(baseOffset: Int, 
                      asm: Assembled,
                      imports: Map[String, Int],
                      externs: Map[String, Int],
                      variables: Map[String, Int]): Array[Byte] = {

    lazy val importNames = imports.keys.toList
    lazy val externNames = externs.keys.toList
    lazy val varNames    = variables.keys.toList
    lazy val procNames   = asm.code.collect{ case BeginProc(name) => name }
    
    def onePass: Seq[Token] = asm.code.flatMap {
        case x: SizedToken => Some(x)
        case x: DynamicSizedToken => Some(x)
        case proc @ BeginProc(_) => Some(proc)
        case JmpRef(name) if externNames.contains(name) => Some(JmpRefResolved(name))
        case Reference(name) if importNames.contains(name) => Some(ImportRef(name))
        case Reference(name) if procNames.contains(name) => Some(ProcRef(name))
        case Reference(name) if varNames.contains(name) => Some(VarRef(name))
        case Reference(_) => throw new Exception("no reference found!")
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
 
    // Build procedure map
    val procs = positionPass.collect{case Proc(offset, name) => (name, offset)}.toMap
    val labels = positionPass.collect{case LabelResolved(offset, name) => (name, offset)}.toMap    
    
    val code: Array[Byte] = {
      var parserPosition = 0
      for (token <- onePass) yield {
        val result = token match {
	        case InstructionToken(inst) => inst.code
	        case Align(to, filler, _) => Array.fill((to - (parserPosition % to)) % to)(filler)
	        case Padding(to, _) => Array.fill(to)(0xCC.toByte)
	        case ProcRef(name) => callNear(*(Immediate32(procs(name) - parserPosition - 5)).rel32).code
	        case VarRef(name) => push(Immediate32(variables(name) + baseOffset)).code
	        case JmpRefResolved(name) => jmp(*(Immediate32((externs(name) + baseOffset)))).code
	        case ImportRef(name) => callNear(*(Immediate32((imports(name) + baseOffset)))).code
	        case LabelRef(name, inst) => inst(Immediate8((labels(name) - parserPosition - 2).toByte)).code
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