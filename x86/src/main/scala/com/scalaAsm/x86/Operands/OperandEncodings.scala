package com.scalaAsm.x86
package Operands

import Memory._
import com.scalaAsm.x86.OpcodeFormat
import com.scalaAsm.x86.Instructions.Sized

object NoOperand

object NoAddressingForm extends InstructionFormat(addressingForm = NoModRM(), immediate = None)

trait OperandFormat

class NoOperandFormat extends ResolvedOneOperand[Constant8](0) {
  def getAddressingForm(op1: Constant8, opcode: OpcodeFormat) = NoAddressingForm
  def getPrefixes(op1: Constant8) = None
}

abstract class TwoOperandFormat[OpEn, -X <: Operand, -Y <: Operand] {
  def apply(operand1Size: Int, operand2Size: Int): ResolvedTwoOperands[X,Y]
}

abstract class OneOperandFormat[OpEn, -X <: Operand] {
  def apply(operand1Size: Int): ResolvedOneOperand[X]
}

abstract class ResolvedOneOperand[-X <: Operand](operand1Size: Int) {
  def getAddressingForm(op1: X, opcode: OpcodeFormat): InstructionFormat
  def getPrefixes(op1: X): Option[Array[Byte]]
}

abstract class ResolvedTwoOperands[-X <: Operand, -Y <: Operand](operand1Size: Int, operand2Size: Int) {
  def getAddressingForm(op1: X, op2: Y, opcode: OpcodeFormat): InstructionFormat
  def getPrefixes(op1: X, op2: Y): Option[Array[Byte]]
}






