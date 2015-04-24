package com.scalaAsm

package object asm {

  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context
  import com.scalaAsm.x86.InstructionResult
  import com.scalaAsm.x86.Operands._
  
  implicit class AsmContext (val sc : StringContext) {
    def asm(args: Any*): InstructionResult = macro AsmMacro.impl
  }
   
  def byte(value: Byte) = Constant8(value)
  def word(value: Short) = Constant16(value)
  def dword(value: Int) = Constant32(value)
  def qword(value: Long) = Constant64(value)
}