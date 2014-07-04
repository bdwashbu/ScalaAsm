package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.NoModRM
import com.scalaAsm.x86.Operands.Memory.InstructionFormat

object NoOperand

object NoAddressingForm extends InstructionFormat(addressingForm = NoModRM(), immediate = None)

abstract class OperandFormat[OpEn, -X <: Product] {
  def getAddressingForm(operands: X, opcode: OpcodeFormat): InstructionFormat
  def getPrefixes(operands: X): Option[Array[Byte]]
}






