package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer
import com.scalaAsm.x86.Instructions._
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.One
import com.scalaAsm.x86.MachineCode
import com.scalaAsm.x86.Operands.Immediate8
import com.scalaAsm.x86.{MachineCodeBuilder1, MachineCodeBuilder2}
import com.scalaAsm.x86.MachineCodeBuilder

trait CodeSection extends Registers with AsmSection[Any] with Catalog {

    case class byte(value: Byte) extends Immediate8 with Displacement8
    case class word(value: Short) extends Immediate16 with Displacement16
    case class dword(value: Int) extends Immediate32 with Displacement32
    case class qword(value: Long) extends Immediate64 with Displacement64

    //implicit def toToken(code: MachineCode) = InstructionToken(code)
    //implicit def toToken[O1](code: MachineCodeBuilder1[O1]) = InstructionToken(code.get)
   // implicit def toToken[O1,O2](code: MachineCodeBuilder2[O1, O2]) = InstructionToken(code.get)

    implicit def toByte(x: Int) = x.toByte
    val One = new One{}
    
    def procedure(name: String, innerCode: Any*) = {
      builder += ProcedureToken(name, innerCode)
    }
    
    def build(code: Seq[Any]): Seq[Any] =
	   code flatMap {
	    case ProcedureToken(name, code) => List(BeginProc(name)) ++ build(code)
	    case CodeGroup(code) => build(code)
	    case token => List(token)
	  }

    def getRawBytes: Array[Byte] = {
//       def getInstTokens(tokens: List[CodeToken]): List[InstructionToken] = tokens.flatMap { x => x match {
//        case Procedure(name, code) => getInstTokens(code)
//        case token: InstructionToken => List(token)
//      }}
//       
       build(builder.toSeq) collect { case x: MachineCodeBuilder => x} map {x => x.get.code} reduce (_ ++ _)
    }
    
    private def procRef(procName: String) = ProcRef(procName)

    def label(name: String) = Label(name)

    def align(to: Int, filler: Byte = 0xCC.toByte) = Align(to, filler, (parserPos) => (to - (parserPos % to)) % to)

    def push(param: String) = Reference(param)

    def jnz(labelRef: String)(implicit ev: JNZ_1[_, Immediate8]) = LabelRef(labelRef, ev) 

    def jz(labelRef: String)(implicit ev: JZ_1[_, Immediate8]) = LabelRef(labelRef, ev)

    def call(refName: String) = Reference(refName)

    def jmp(ref: String) = JmpRef(ref)

    def repeat(numTimes: Int, code: List[Any]): CodeGroup = {
      val expanded = ListBuffer[Any]()
      for (i <- 0 until numTimes) {
        expanded ++= code
      }
      CodeGroup(expanded.toList)
    }
}