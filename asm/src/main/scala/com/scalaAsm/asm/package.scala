package com.scalaAsm

package object asm {

  import scala.language.experimental.macros

  import scala.reflect.macros.blackbox.Context
  import com.scalaAsm.x86.InstructionResult
  import com.scalaAsm.x86.Operands.Constant
  import com.scalaAsm.x86
  
  implicit class AsmContext (val sc : StringContext) {
    def asm(args: Any*): InstructionResult = macro AsmMacro.impl
  }
   
  def byte(value: Byte) = Constant(value)
  def word(value: Short) = Constant(value)
  def dword(value: Int) = Constant(value)
  def qword(value: Long) = Constant(value)
}