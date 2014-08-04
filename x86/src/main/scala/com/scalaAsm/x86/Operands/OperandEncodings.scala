package com.scalaAsm.x86
package Operands

import Memory._
import com.scalaAsm.x86.OpcodeFormat

object NoOperand

abstract class NoAddressingFormFormat extends InstructionFormat(addressingForm = NoModRM(), immediate = None)
object NoAddressingForm extends NoAddressingFormFormat

trait OperandFormat

class NoOperandFormat[OpEn, -X <: Operand] extends OneOperandFormat[OpEn, X] {
  type AddressInputs = Unit
  def getAddressingForm(opcode: OpcodeFormat) = {(Unit) => new NoAddressingFormFormat{}}
  def getPrefixes(op1: X) = None
}

abstract class TwoOperandFormat[OpEn, -X <: Operand, -Y <: Operand] {
  def getAddressingForm(op1: X, op2: Y, opcode: OpcodeFormat): InstructionFormat
  def getPrefixes(op1: X, op2: Y): Option[Array[Byte]]
}

abstract class OneOperandFormat[OpEn, -X <: Operand] {
  def getAddressingForm(opcode: OpcodeFormat): X => InstructionFormat
  def getPrefixes(op1: X): Option[Array[Byte]]
}






