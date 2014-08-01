package com.scalaAsm.x86

import Operands._
import Operands.Memory.NoModRM
import Operands.Memory.InstructionFormat

object NoOperand

object NoAddressingForm extends InstructionFormat(addressingForm = NoModRM(), immediate = None)

trait OperandFormat

class NoOperandFormat[OpEn, -X <: Operand] extends OneOperandFormat[OpEn, X] {
  def getAddressingForm(op1: X, opcode: OpcodeFormat) = NoAddressingForm
  def getPrefixes(op1: X) = None
}

abstract class TwoOperandFormat[OpEn, -X <: Operand, -Y <: Operand] {
  def getAddressingForm(op1: X, op2: Y, opcode: OpcodeFormat): InstructionFormat
  def getPrefixes(op1: X, op2: Y): Option[Array[Byte]]
}

abstract class OneOperandFormat[OpEn, -X <: Operand] {
  def getAddressingForm(op1: X, opcode: OpcodeFormat): InstructionFormat
  def getPrefixes(op1: X): Option[Array[Byte]]
}






