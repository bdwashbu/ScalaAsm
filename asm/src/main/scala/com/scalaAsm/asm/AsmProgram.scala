package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer
import com.scalaAsm.asm.Tokens.Token
import scala.collection.mutable.ListBuffer
import com.scalaAsm.x86.Instructions.{ Standard, Formats }
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.x86.InstructionResult
import scala.language.implicitConversions
import java.nio.ByteBuffer
import com.scalaAsm.x86.Instructions.Standard._

import com.scalaAsm.x86.Instructions.Catalog
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86._

trait x86Mode
trait x86_64 extends x86_32
trait x86_32 extends x86Mode

trait AsmSection

trait AsmProgram[Mode <: x86Mode] {

  val sections = new ListBuffer[AsmSection]()
  
  def procNames = codeTokens.collect { case BeginProc(name) => name }
  def codeTokens = sections.collect { case (x: AsmProgram[_]#CodeSection) => x }.flatMap { seg => seg.build(seg.builder.toSeq) }
  def varTokens = sections.collect { case (x: DataSection) => x }.flatMap { seg => seg.tokens }
  def variableNames = varTokens.collect { case Variable(name,_) => name }
  
  def Op[X](from: X) = new Operand[X] {
      def apply = from
      override def toString = from.toString
    }

  trait CodeSection extends Registers[Mode] with Catalog.Standard with Formats with Addressing with AsmSection {

    val builder = new ListBuffer[InstructionResult]()

    def byte(value: Byte) = Op(Constant8(value))
    def word(value: Short) = Op(Constant16(value))
    def dword(value: Int) = Op(Constant32(value))
    def qword(value: Long) = Op(Constant64(value))

    implicit def toByte(x: Int) = x.toByte
    val One = new One {}

    def procedure(name: String, innerCode: InstructionResult*) = {
      builder += ProcedureToken(name, innerCode)
    }
    
    case class Code(code: InstructionResult*) extends InstructionResult {
      def mnemonic = ""
      def getSize = 0
      def getBytes = Array()
    }

    def build(code: Seq[InstructionResult]): Seq[InstructionResult] =
      code flatMap {
        case ProcedureToken(name, code) => BeginProc(name) +: build(code)
        case Code(codes @ _*)           => build(codes)
        case token                      => List(token)
      }

    def getRawBytes: Array[Byte] = {
      build(builder.toSeq) collect { case x: InstructionResult => x } map { x => x.getBytes } reduce (_ ++ _)
    }

    private def procRef(procName: String) = ProcRef(procName)

    

    def addr(varName: String) = {
      Op(new AbsoluteAddress[_32] {
        var offset = 0
        def getRelative = null
        def apply = Constant32(0)
        val name = Some(varName)
      })
    }

    def label(name: String) = Label(name)

    def align(to: Int, filler: Byte = 0xCC.toByte) = Align(to, filler, (parserPos) => (to - (parserPos % to)) % to)

    def push(param: String) = Reference(param)

    def jnz(ref: String)(implicit ev: JNZ._1[Constant8], format: OneOperandFormat[Constant8]) = LabelRef(ref, ev, format)

    def jz(ref: String)(implicit ev: JZ._1[Constant8], format: OneOperandFormat[Constant8]) = LabelRef(ref, ev, format)

    def jl(ref: String)(implicit ev: JL._1[Constant8], format: OneOperandFormat[Constant8]) = LabelRef(ref, ev, format)

    def je(ref: String)(implicit ev: JE._1[Constant8], format: OneOperandFormat[Constant8]) = LabelRef(ref, ev, format)

    def call(refName: String) = Reference(refName)

    def invoke(refName: String) = Invoke(refName)

    def jmp(ref: String)(implicit ev: JMP._1[Constant8], format: OneOperandFormat[Constant8]) = LabelRef(ref, ev, format)

    def repeat(numTimes: Int, code: List[InstructionResult]): Code = {
      val expanded = ListBuffer[InstructionResult]()
      for (i <- 0 until numTimes) {
        expanded ++= code
      }
      Code(expanded: _*)
    }
  }
}