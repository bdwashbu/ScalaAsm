package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer
import com.scalaAsm.asm.Tokens.Token
import scala.collection.mutable.ListBuffer
import com.scalaAsm.x86.Instructions.{Standard, Formats}
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.x86.Operands._
import scala.language.implicitConversions
import java.nio.ByteBuffer
import com.scalaAsm.x86.InstructionResult
import com.scalaAsm.x86.Instructions.Standard.{JNZ_1, JZ_1}
import com.scalaAsm.x86.Instructions.`package`.OneOperandEncoding
import com.scalaAsm.x86.Instructions.`package`.I

trait AsmProgram[Mode <: x86Mode] {

  val codeSections = new ListBuffer[CodeSection]()
  val dataSections = new ListBuffer[DataSection]()

  trait CodeSection extends Registers[Mode] with AsmSection[InstructionResult] with Standard.Catalog with Formats with Addressing {

    def byte(value: Byte) = Op(Constant8(value))
    def word(value: Short) = Op(Constant16(value))
    def dword(value: Int) = Op(Constant32(value))
    def qword(value: Long) = Op(Constant64(value))

    implicit def toByte(x: Int) = x.toByte
    val One = new One {}

    def procedure(name: String, innerCode: InstructionResult*) = {
      builder += ProcedureToken(name, innerCode)
    }

    def build(code: Seq[InstructionResult]): Seq[InstructionResult] =
      code flatMap {
        case ProcedureToken(name, code) => BeginProc(name) +: build(code)
        case CodeGroup(code) => build(code)
        case token => List(token)
      }

    def getRawBytes: Array[Byte] = {
      build(builder.toSeq) collect { case x: InstructionResult => x } map { x => x.getBytes } reduce (_ ++ _)
    }

    private def procRef(procName: String) = ProcRef(procName)

    def label(name: String) = Label(name)

    def align(to: Int, filler: Byte = 0xCC.toByte) = Align(to, filler, (parserPos) => (to - (parserPos % to)) % to)

    def push(param: String) = Reference(param)

    def jnz(labelRef: String)(implicit ev: JNZ_1[Constant8, I], format: OneOperandFormat[Constant8, I]) = LabelRef(labelRef, ev, format)

    def jz(labelRef: String)(implicit ev: JZ_1[Constant8, I], format: OneOperandFormat[Constant8, I]) = LabelRef(labelRef, ev, format)

    def call(refName: String) = Reference(refName)

    def invoke(refName: String) = Invoke(refName)

    def jmp(ref: String) = JmpRef(ref)

    def repeat(numTimes: Int, code: List[InstructionResult]): CodeGroup = {
      val expanded = ListBuffer[InstructionResult]()
      for (i <- 0 until numTimes) {
        expanded ++= code
      }
      CodeGroup(expanded.toList)
    }
  }
}