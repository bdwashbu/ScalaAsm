package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer
import com.scalaAsm.x86.Instructions._
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.One
import com.scalaAsm.x86.MachineCode
import com.scalaAsm.x86.MachineCodeBuilder
import com.scalaAsm.x86.Operands.Memory._
import scala.language.implicitConversions
import java.nio.ByteBuffer

// Standard real-mode dos program that prints an error if it cannot be run

object DosStub {
  val dosWarning = "This program cannot be run in DOS mode.\r\r\n$"
  def putStub: Array[Byte] = {
    
    val dosStub = new CodeSection {
      builder += push(cs)
      builder += pop(ds)
      builder += mov(dx, word(0xE.toByte))
      builder += mov(ah, byte(0x9))
      builder += int(byte(0x21))
      builder += mov(ax, word(0x4C01))
      builder += int(byte(0x21))
    }

    val bbuf = ByteBuffer.allocate(dosStub.getRawBytes.length + dosWarning.length);
    bbuf.put(dosStub.getRawBytes)
    bbuf.put(dosWarning.toCharArray() map (_.toByte))
    bbuf.array()
  }
}

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
	    case ProcedureToken(name, code) => List(BeginProc(name)) ++ build(code)
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

    def jnz(labelRef: String)(implicit ev: JNZ_1[_, Constant8]) = LabelRef(labelRef, ev) 

    def jz(labelRef: String)(implicit ev: JZ_1[_, Constant8]) = LabelRef(labelRef, ev)

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