package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.NoModRM
import com.scalaAsm.x86.Operands.Memory.InstructionConstants

trait OperandFormat

object NoOperand extends OperandFormat

object NoAddressingForm extends InstructionConstants(addressingForm = NoModRM(), displacement = None, immediate = None)

abstract class OneOperandFormat[OpEn, -X <: Operand] extends OperandFormat {
  def getAddressingForm(x: X, opcode: OpcodeFormat): InstructionConstants
}

abstract class TwoOperandsFormat[OpEn, -X <: Operand, -Y <: Operand] extends OperandFormat {
  def getPrefixes(x: X, y: Y): Option[Array[Byte]]
  def getAddressingForm(x: X, y: Y, opcode: OpcodeFormat): InstructionConstants
}





