package com.scalaAsm.x86
package Operands

import Memory._
import com.scalaAsm.x86.OpcodeFormat

object NoOperand

object NoAddressingForm extends InstructionFormat(addressingForm = NoModRM(), immediate = None)

trait OperandFormat

class NoOperandFormat extends ResolvedOneOperand[Constant8](0, null, Array()) {
  def getAddressingForm(op1: Constant8) = NoAddressingForm
  val size = 1
}


abstract class TwoOperandFormat[OpEn, -X, -Y] extends Function4[Int, Int, OpcodeFormat, Array[Byte], ResolvedTwoOperands[X,Y]]
abstract class OneOperandFormat[OpEn, -X] extends Function3[Int, OpcodeFormat, Array[Byte], ResolvedOneOperand[X]]

abstract class ResolvedOneOperand[-X](operand1Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) {
  def getAddressingForm(op1: X): InstructionFormat
  def getPrefix = prefix
  def size: Int
}



abstract class ResolvedTwoOperands[-X, -Y](operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) {
  def getAddressingForm(op1: X, op2: Y): InstructionFormat
  def getPrefix = prefix
  def size: Int
}






