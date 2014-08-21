package com.scalaAsm.x86
package Operands

import Memory._
import com.scalaAsm.x86.OpcodeFormat

object NoOperand

object NoAddressingForm extends InstructionFormat(addressingForm = NoModRM(), immediate = None)

trait OperandFormat

class NoOperandFormat extends ResolvedOneOperand[Constant8](0, null) {
  def getAddressingForm(op1: Constant8) = NoAddressingForm
  override def getPrefixes(op1: Constant8) = None
  val size = 1
}

abstract class TwoOperandFormat[OpEn, -X, -Y] {
  def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat): ResolvedTwoOperands[X,Y]
}

abstract class OneOperandFormat[OpEn, -X] {
  def apply(operand1Size: Int, opcode: OpcodeFormat): ResolvedOneOperand[X]
}

abstract class ResolvedOneOperand[-X](operand1Size: Int, opcode: OpcodeFormat) {
  def getAddressingForm(op1: X): InstructionFormat
  def getPrefixes(op1: X): Option[Array[Byte]] = None
  def size: Int
}

abstract class ResolvedTwoOperands[-X, -Y](operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat) {
  def getAddressingForm(op1: X, op2: Y): InstructionFormat
  def getPrefixes(op1: X, op2: Y): Option[Array[Byte]] = None
  def size: Int
}






