package com.scalaAsm

package object asm {

  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context
  import com.scalaAsm.x86.InstructionResult
  import com.scalaAsm.x86.Operands._
  
  implicit class AsmContext (val sc : StringContext) {
    def asm(args: Any*): InstructionResult = macro AsmMacro.impl
  }
  
  def Op[X](from: X) = new Operand[X] {
    def apply = from
    override def toString = from.toString
  }
   
  def byte(value: Byte) = Op(Constant8(value))
  def word(value: Short) = Op(Constant16(value))
  def dword(value: Int) = Op(Constant32(value))
  def qword(value: Long) = Op(Constant64(value))
}