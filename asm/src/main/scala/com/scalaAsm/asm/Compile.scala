package com.scalaAsm.asm

import com.scalaAsm.asm.Tokens._
import com.scalaAsm.utils.Endian
import com.scalaAsm.x86.Instructions.CALL
import com.scalaAsm.x86.Operands.{Immediate16, Immediate32}
import com.scalaAsm.asm.Addressing._
import com.scalaAsm.x86.Instructions.JMP
import com.scalaAsm.x86.Instructions.PUSH


object AsmCompiler 
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
        case JmpRef(name) if externNames.contains(name) => Some(JmpRefResolved(name))
        case Reference(name) if importNames.contains(name) => Some(ImportRef(name))
        case Reference(name) if procNames.contains(name) => Some(ProcRef(name))
        case Reference(name) if varNames.contains(name) => Some(VarRef(name))
        case Reference(_) => throw new Exception("no reference found!")
        case Label(name) => Some(Label(name))
        case LabelRef(name,opcode) => Some(LabelRef(name,opcode))
        case _ => None
    }

    def twoPass: Seq[PostToken] = {
      var parserPosition = 0
      for (token <- onePass) yield {
        val result = token match {
	        case InstructionToken(inst) => ByteOutputPost(inst.code)
	        case Align(to, filler, _) => ByteOutputPost(Array.fill((to - (parserPosition % to)) % to)(filler))
	        case Padding(to, _) => ByteOutputPost(Array.fill(to)(0xCC.toByte))
	        case BeginProc(name) => Proc(parserPosition, name)
	        case ProcRef(name) => ProcState(parserPosition, name)
	        case VarRef(name) => VarState(parserPosition, name)
	        case JmpRefResolved(name) => JmpState(parserPosition, name)
	        case ImportRef(name) => ImportState(parserPosition, name)
	        case Label(name) => LabelResolved(parserPosition, name)
	        case LabelRef(name, opCode) => LabelRefResolved(parserPosition, name, opCode)
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
    val procs = twoPass.collect{case Proc(offset, name) => (name, offset)}.toMap
    val labels = twoPass.collect{case LabelResolved(offset, name) => (name, offset)}.toMap

    val code: Array[Byte] = twoPass.map {
        case ByteOutputPost(code) => code
        case ProcState(offset, name) => CALL.callNear(*(Immediate32(procs(name) - offset - 5)).rel32).build.code
        case VarState(_, name) => PUSH.push(Immediate32(variables(name) + baseOffset)).build.code
        case ImportState(_, name) => CALL.callNear(*(Immediate32((imports(name) + baseOffset)))).build.code
        case JmpState(_, name) => JMP.jmp(*(Immediate32((externs(name) + baseOffset)))).build.code
        case LabelRefResolved(offset, name, opCode) => Array(opCode, (labels(name) - offset - 2).toByte)
        case _ => Array[Byte]()
    }.reduce(_ ++ _)
    
    code 
  }
}