package com.scalaAsm.x86
package Operands

import Memory._
import com.scalaAsm.x86.OpcodeFormat
import com.scalaAsm.x86.Instructions.`package`.TwoOperandEncoding
import com.scalaAsm.x86.Instructions.`package`.OneOperandEncoding

object NoOperand

object NoAddressingForm extends InstructionFormat(addressingForm = NoModRM(), immediate = None)

trait OperandFormat

class NoOperandFormat extends ResolvedZeroOperand(null, Seq()) {
  val size = 1
}


abstract class TwoOperandFormat[-X, -Y, -OpEn <: TwoOperandEncoding[X,Y]] {
  def getAddressingForm(op1: X, op2: Y, opcode: OpcodeFormat): InstructionFormat
  def getPrefix(prefix: Seq[Prefix]): Array[Byte] = prefix.map(_.get).foldLeft(Array[Byte]()){ _ ++ _ }
  def size(opcode: OpcodeFormat, prefix: Seq[Prefix]): Int
}

abstract class OneOperandFormat[-X, -OpEn <: OneOperandEncoding[X]] {
  def getAddressingForm(op1: X, opcode: OpcodeFormat): InstructionFormat
  def getPrefix(prefix: Seq[Prefix]): Array[Byte] = prefix.map(_.get).foldLeft(Array[Byte]()){ _ ++ _ }
  def size(opcode: OpcodeFormat, prefix: Seq[Prefix]): Int
}

abstract class ResolvedZeroOperand(opcode: OpcodeFormat, prefix: Seq[Prefix]) {
  def getPrefix: Array[Byte] = Array()
  def size: Int
}




