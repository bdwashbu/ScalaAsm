package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer
import com.scalaAsm.x86.Instructions.Standard._
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.MachineCode
import scala.language.implicitConversions
import java.nio.ByteBuffer
import com.scalaAsm.x86.Instructions.MachineCodeBuilder
import com.scalaAsm.x86.Instructions.Catalog

trait CodeSection extends Registers with AsmSection[Any] with Catalog {

    def byte(value: Byte) = Constant8(value)
    def word(value: Short) = Constant16(value)
    def dword(value: Int) = Constant32(value)
    def qword(value: Long) = Constant64(value)

    implicit def toByte(x: Int) = x.toByte
    val One = new One{}
    
    def procedure(name: String, innerCode: Any*) = {
      builder += ProcedureToken(name, innerCode)
    }
    
    def build(code: Seq[Any]): Seq[Any] =
	   code flatMap {
	    case ProcedureToken(name, code) => BeginProc(name) +: build(code)
	    case CodeGroup(code) => build(code)
	    case token => List(token)
	  }

    def getRawBytes: Array[Byte] = {
       build(builder.toSeq) collect { case x: MachineCodeBuilder => x} map {x => x.get.code} reduce (_ ++ _)
    }
    
    private def procRef(procName: String) = ProcRef(procName)

    def label(name: String) = Label(name)

    def align(to: Int, filler: Byte = 0xCC.toByte) = Align(to, filler, (parserPos) => (to - (parserPos % to)) % to)

    def push(param: String) = Reference(param)

    def jnz[OpEn](labelRef: String)(implicit ev: JNZ_1[OpEn, Constant8], format: OneOperandFormat[OpEn, Constant8]) = LabelRef(labelRef, ev, format) 

    def jz[OpEn](labelRef: String)(implicit ev: JZ_1[OpEn, Constant8], format: OneOperandFormat[OpEn, Constant8]) = LabelRef(labelRef, ev, format)

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