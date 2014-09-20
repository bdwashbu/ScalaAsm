package com.scalaAsm.x86
package Operands

import Memory._
import com.scalaAsm.x86.OpcodeFormat
import com.scalaAsm.x86.Instructions.`package`.TwoOperandEncoding
import com.scalaAsm.x86.Instructions.`package`.OneOperandEncoding

object NoOperand

object NoAddressingForm extends InstructionFormat(addressingForm = NoModRM(), immediate = None)

trait OperandFormat

class NoOperandFormat extends ResolvedZeroOperand(null, Array()) {
  val size = 1
}


abstract class TwoOperandFormat[-X, -Y, -OpEn <: TwoOperandEncoding[X,Y]] extends Function2[OpcodeFormat, Array[Byte], ResolvedTwoOperands[X,Y]]
abstract class OneOperandFormat[-X, -OpEn <: OneOperandEncoding[X]] extends Function2[OpcodeFormat, Array[Byte], ResolvedOneOperand[X]]

abstract class ResolvedZeroOperand(opcode: OpcodeFormat, prefix: Array[Byte]) {
  def getPrefix = prefix
  def size: Int
}

abstract class ResolvedOneOperand[-X](opcode: OpcodeFormat, prefix: Array[Byte]) {
  def getAddressingForm(op1: X): InstructionFormat
  def getPrefix = prefix
  def size: Int
}



abstract class ResolvedTwoOperands[-X, -Y](opcode: OpcodeFormat, prefix: Array[Byte]) {
  def getAddressingForm(op1: X, op2: Y): InstructionFormat
  def getPrefix = prefix
  def size: Int
}






