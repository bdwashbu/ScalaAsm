package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._

trait InstructionField {
  def getBytes: Array[Byte]
  def size: Int
}

trait x86Instruction {
  import scala.language.implicitConversions
  val mnemonic: String
  def prefix = Seq[Prefix]()

  implicit def toPrefixSeq(x: Prefix) = Seq(x)
  implicit def toByte(x: Int) = x.toByte
  implicit def toOneOpcode(x: Int): OneOpcode = OneOpcode(x.toByte, prefix)
  implicit def toTwoOpcodes(x: (Int, Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte, prefix)
}

trait OneOperand[X <: InstructionDefinition[_]] {
  def apply[O1](p1: Operand[O1])(implicit ev: X#_1[O1], format: OneOperandFormat[O1]) = ev(p1, format, ev.prefix)
}

trait TwoOperands[X <: InstructionDefinition[_]] {
  def apply[O1, O2](p1: Operand[O1], p2: Operand[O2])(implicit ev: X#_2[O1, O2], format: TwoOperandFormat[O1, O2]) = ev(p1, p2, format)
}

class ZeroOperands[X <: InstructionDefinition[_]] {
  def apply(ignored: Unit)(implicit ev: X#_0) = ev.get
}

case class ZeroMachineCode(format: ResolvedZeroOperand, opcode: OpcodeFormat, mnemonic: String) extends InstructionResult {

  def getSize: Int = {
    format.getPrefix.size + opcode.size
  }

  def getBytes: Array[Byte] = {
    format.getPrefix ++: opcode.get
  }
}

case class OneMachineCode[O1, OpEn](
  operand: O1,
  instBytes: Array[Byte],
  prefixAndOpcode: Array[Byte],
  mnemonic: String,
  opcodeExtension: Byte) extends InstructionResult {

  override def toString = {
    val formattedMnemonic = mnemonic.head + mnemonic.tail.toLowerCase()
    formattedMnemonic + " " + operand.toString
  }

  def getSize: Int = {
    prefixAndOpcode.size + instBytes.size
  }

  def getBytes: Array[Byte] = {
    prefixAndOpcode ++: instBytes
  }
}

case class TwoMachineCode[O1, O2, OpEn](
  operand: O1,
  operand2: O2,
  instBytes: Array[Byte],
  prefixAndOpcode: Array[Byte],
  mnemonic: String) extends InstructionResult {

  override def toString = {
    val formattedMnemonic = mnemonic.head + mnemonic.tail.toLowerCase()
    formattedMnemonic + " " + operand.toString + ", " + operand2.toString
  }

  def getSize: Int = {
    prefixAndOpcode.size + instBytes.size
  }

  def getBytes: Array[Byte] = {
    prefixAndOpcode ++: instBytes
  }
}


