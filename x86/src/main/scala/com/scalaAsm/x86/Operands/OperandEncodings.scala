package com.scalaAsm.x86
package Operands

import Memory._
import com.scalaAsm.x86.OpcodeFormat
import com.scalaAsm.x86.Instructions.Sized

object NoOperand

object NoAddressingForm extends InstructionFormat(addressingForm = NoModRM(), immediate = None)

trait OperandFormat

class NoOperandFormat extends ResolvedOneOperand[Constant8](0, null) {
  def getAddressingForm(op1: Constant8) = NoAddressingForm
  def getPrefixes(op1: Constant8) = None
}

abstract class TwoOperandFormat[OpEn, -X <: Operand, -Y <: Operand] {
  def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat): ResolvedTwoOperands[X,Y]
}

abstract class OneOperandFormat[OpEn, -X <: Operand] {
  def apply(operand1Size: Int, opcode: OpcodeFormat): ResolvedOneOperand[X]
}

abstract class ResolvedOneOperand[-X <: Operand](operand1Size: Int, opcode: OpcodeFormat) {
  def getAddressingForm(op1: X): InstructionFormat
  def getPrefixes(op1: X): Option[Array[Byte]]
}

abstract class ResolvedTwoOperands[-X <: Operand, -Y <: Operand](operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat) {
  def getAddressingForm(op1: X, op2: Y): InstructionFormat
  def getPrefixes(op1: X, op2: Y): Option[Array[Byte]]
}






