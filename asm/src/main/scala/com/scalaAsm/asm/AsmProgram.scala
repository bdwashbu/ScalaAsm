package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer
import com.scalaAsm.asm.Tokens.Token
import scala.collection.mutable.ListBuffer
import com.scalaAsm.x86.Instructions.Formats
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.x86.InstructionResult
import scala.language.implicitConversions
import java.nio.ByteBuffer
import com.scalaAsm.x86.Instructions.General._

import com.scalaAsm.x86.Instructions.Catalog
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait x86Mode
trait x86_64 extends x86_32
trait x86_32 extends x86Mode

trait AsmSection
 trait HighLevel

trait AsmProgram[Mode <: x86Mode] extends Formats {

  val sections = new ListBuffer[AsmSection]()
  
  lazy val procNames = procTokens.collect { case ProcedureToken(name,_) => name }
  lazy val procTokens = sections.collect { case (x: AsmProgram[_]#CodeSection) => x }.flatMap { seg => seg.builder.toSeq }
  lazy val codeTokens = sections.collect { case (x: AsmProgram[_]#CodeSection) => x }.flatMap { seg => seg.build(seg.builder.toSeq) }
  def varTokens = sections.collect { case (x: DataSection) => x }.flatMap { seg => seg.tokens }
  def variableNames = varTokens.collect { case Variable(name,_) => name }
  
  def Op[X](from: X) = new Operand[X] {
      def apply = from
      override def toString = from.toString
    }
  
  val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
  import universe._
  
  implicit class OctalContext (val sc : StringContext) {
    def asm(): () => InstructionResult = macro AsmMacro.impl
  }
  
  trait CodeSection extends Registers[Mode] with Catalog.Standard  with Addressing with AsmSection {

    val builder = new ListBuffer[HighLevel]()

    def byte(value: Byte) = Op(Constant8(value))
    def word(value: Short) = Op(Constant16(value))
    def dword(value: Int) = Op(Constant32(value))
    def qword(value: Long) = Op(Constant64(value))

    implicit def toByte(x: Int) = x.toByte
    val One = new One {}

    def procedure(name: String, innerCode: (() => InstructionResult)*) = {
      builder += ProcedureToken(name, innerCode)
    }
    
    case class Code(code: (() => InstructionResult)*) extends HighLevel with InstructionResult {
      def mnemonic = ""
      def apply = Array()
    }

    def build(code: Seq[HighLevel]): Seq[InstructionResult] = {
      
      def buildLow(code: Seq[() => InstructionResult]): Seq[InstructionResult] = {
        code.map(_.apply) flatMap {
          case token                      => List(token)
        }
      }
      
      code flatMap {
        case ProcedureToken(name, code) => BeginProc(name) +: buildLow(code)
        case Code(codes @ _*)           => buildLow(codes)
        case align @ Align(_,_,_)       => List(align)
      }
    }

    def getRawBytes: Array[Byte] = {
      build(builder.toSeq) collect { case x: InstructionResult => x } map { x => x.apply } reduce (_ ++ _)
    }

    private def procRef(procName: String) = ProcRef(procName)
    
    def addr(varName: String): Operand[AbsoluteAddress[_32]] = {
      Op(new AbsoluteAddress[_32] {
        var offset = 0
        def getRelative = null
        def apply = Constant32(0)
        val name = Some(varName)
      })
    }
    
    def addr64(varName: String): Operand[AbsoluteAddress[_64]] = {
      Op(new AbsoluteAddress[_64] {
        var offset = 0.toLong
        def getRelative = null
        def apply = Constant64(0)
        val name = Some(varName)
      })
    }

    def label(name: String) = () => Label(name)

    def align(to: Int, filler: Byte = 0xCC.toByte) = Align(to, filler, (parserPos) => (to - (parserPos % to)) % to)

    def push(param: String) = () => Reference(param)
    
    def jnz(ref: String)(implicit ev: JNZ#_1[Constant8], format: OneOperandFormat[Constant8]) = () => LabelRef(ref, ev, format)

    def jz[T <: JZ](ref: String)(implicit ev: T#_1[Constant8], format: OneOperandFormat[Constant8]) = () => LabelRef(ref, ev, format)

    def jl(ref: String)(implicit ev: JL#_1[Constant8], format: OneOperandFormat[Constant8]) = () => LabelRef(ref, ev, format)

    def je(ref: String)(implicit ev: JE#_1[Constant8], format: OneOperandFormat[Constant8]) = () => LabelRef(ref, ev, format)

    def call(refName: String): () => InstructionResult = () => {
      if (procNames.contains(refName)) {
        ProcRef(refName)
      } else {
        ImportRef(0, refName)
      }
    }

    def invoke(refName: String) = () => Invoke(refName)

    def jmp(ref: String)(implicit ev: JMP#_1[Constant8], format: OneOperandFormat[Constant8]) = () => LabelRef(ref, ev, format)

    def repeat(numTimes: Int, code: List[() => InstructionResult]): () => Code = {
      val expanded = ListBuffer[() => InstructionResult]()
      for (i <- 0 until numTimes) {
        expanded ++= code
      }
      () => Code(expanded: _*)
    }
  }
}