package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

trait OperandFormat

object NoOperand extends OperandFormat

object NoAddressingForm extends AddressingFormSpecifierTemp {
  val (addressingForm, displacment, immediate) = (NoModRM(), None, None)
}

abstract class OneOperandFormat[OpEn, -X <: Operand] extends OperandFormat {
  def getAddressingForm(x: X, opcode: OpcodeFormat): AddressingFormSpecifierTemp
}

abstract class TwoOperandsFormat[OpEn, -X <: Operand, -Y <: Operand] extends OperandFormat {
  def getPrefixes(x: X, y: Y): Option[Array[Byte]]
  def getAddressingForm(x: X, y: Y, opcode: OpcodeFormat): AddressingFormSpecifierTemp
}





