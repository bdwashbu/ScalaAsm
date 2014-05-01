package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer
import com.scalaAsm.x86.Instructions._
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.One
import com.scalaAsm.x86.MachineCode

trait CodeSection extends Registers with AsmSection[CodeToken] with Catalog {

    def imm8(x: Byte) = Immediate8(x)
    def imm16(x: Short) = Immediate16(x)
    def imm32(x: Int) = Immediate32(x)
    def imm64(x: Long) = Immediate64(x)
    
    def byte(x: Byte) = Displacement8(x)
    def word(x: Short) = Displacement16(x)
    def dword(x: Int) = Displacement32(x)
    def qword(x: Long) = Displacement64(x)

    implicit def toToken(code: MachineCode) = InstructionToken(code)

    implicit def toByte(x: Int) = x.toByte
    val One = new One{}
    
    def procedure(name: String, innerCode: CodeToken*) = {
      builder += ProcedureToken(name, innerCode)
    }
    
    def build(code: Seq[CodeToken]): Seq[Token] =
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
       build(builder.toSeq) collect { case x: InstructionToken => x} map {x => x.inst.code} reduce (_ ++ _)
    }
    
    private def procRef(procName: String) = ProcRef(procName)

    def label(name: String) = Label(name)

    def align(to: Int, filler: Byte = 0xCC.toByte) = Align(to, filler, (parserPos) => (to - (parserPos % to)) % to)

    def push(param: String) = Reference(param)

    def jnz(labelRef: String)(implicit ev: JNZ_1[Immediate8]) = LabelRef(labelRef, ev) 

    def jz(labelRef: String)(implicit ev: JZ_1[Immediate8]) = LabelRef(labelRef, ev)

    def call(refName: String) = Reference(refName)

    def jmp(ref: String) = JmpRef(ref)

    def repeat(numTimes: Int, code: List[CodeToken]): CodeGroup = {
      val expanded = ListBuffer[CodeToken]()
      for (i <- 0 until numTimes) {
        expanded ++= code
      }
      CodeGroup(expanded.toList)
    }
}