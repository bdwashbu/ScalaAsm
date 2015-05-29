package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Instructions._

trait InstructionField {
  def getBytes: Array[Byte]
  def size: Int
}

trait x86Instruction {
  import scala.language.implicitConversions
  val mnemonic: String
  def opcode: OpcodeFormat
  def prefix = Seq[Prefix]()
  
  implicit def toPrefixSeq(x: Prefix) = Seq(x)
  implicit def toByte(x: Int) = x.toByte
  implicit def toOneOpcode(x: Int): OneOpcode = OneOpcode(x.toByte, prefix, false)
  implicit def toTwoOpcodes(x: (Int, Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte, prefix, false)
  implicit def toThreeOpcodes(x: (Int, Int, Int)): ThreeOpcodes = ThreeOpcodes(x._1.toByte, x._2.toByte, x._3.toByte, prefix, false)
}

trait OneOperand[X <: InstructionDefinition] {
  def apply[O1](p1: O1)(implicit ev: X#_1[O1], format: OneOperandFormat[O1]): OneMachineCode[O1] = ev(p1, format, ev.prefix)
}

trait TwoOperands[X <: InstructionDefinition] {
  def apply[O1, O2](p1: O1, p2: O2)(implicit ev: X#_2[O1, O2], format: TwoOperandFormat[O1, O2]): TwoMachineCode[O1, O2] = ev(p1, p2, format)
}

trait ZeroOperands[X <: InstructionDefinition] {
  def apply(ignored: Unit)(implicit ev: X#_0): ZeroMachineCode = ev.get
}

object NoAddressingForm extends InstructionFormat(addressingForm = NoModRM(), immediate = Array())

case class ZeroMachineCode(format: ResolvedZeroOperand, opcode: OpcodeFormat, mnemonic: String) extends InstructionResult {

  def apply: Array[Byte] = {
    format.getPrefix ++: opcode.get
  }
}

case class OneMachineCode[O1] (
  opcode: OpcodeFormat,
  operand: O1,
  prefixBytes: Array[Byte],
  mnemonic: String,
  explicitFormat: (O1) => Option[InstructionFormat],
  implicitFormat: OneOperandFormat[O1]) extends InstructionResult {

  override def toString = {
    val formattedMnemonic = mnemonic.head + mnemonic.tail.toLowerCase()
    formattedMnemonic + " " + operand.toString
  }

  def apply: Array[Byte] = {
    val opEx: Byte = if (!opcode.opcodeExtension.isEmpty) opcode.opcodeExtension.get else -1
    
    val prefixAndOpcode = if (opcode.isInstanceOf[OpcodeWithReg] && operand.isInstanceOf[reg]) { // this is hacky as hell!
      val opcodePlus = opcode.asInstanceOf[OpcodeWithReg]
      opcodePlus.reg = operand.asInstanceOf[reg]
      prefixBytes ++: opcodePlus.get
    } else {
      prefixBytes ++: opcode.get
    }
    
    val addressForm = explicitFormat(operand) getOrElse implicitFormat.getAddressingForm(operand, opEx)
    
    prefixAndOpcode ++: addressForm.getBytes
  }
}

case class TwoMachineCode[O1, O2] (
  opcode: OpcodeFormat,
  operand: O1,
  operand2: O2,
  prefixBytes: Array[Byte],
  mnemonic: String,
  explicitFormat: (O1, O2) => Option[InstructionFormat],
  implicitFormat: TwoOperandFormat[O1, O2]) extends InstructionResult {

  override def toString = {
    val formattedMnemonic = mnemonic.head + mnemonic.tail.toLowerCase()
      formattedMnemonic + " " + operand.toString + ", " + operand2.toString
    }
  
  def apply: Array[Byte] = {
    val opEx: Byte = if (!opcode.opcodeExtension.isEmpty) opcode.opcodeExtension.get else -1
    
    val prefixAndOpcode = if (opcode.isInstanceOf[OpcodeWithReg] && operand.isInstanceOf[reg]) { // this is hacky as hell!
      val opcodePlus = opcode.asInstanceOf[OpcodeWithReg]
      opcodePlus.reg = operand.asInstanceOf[reg]
      prefixBytes ++: opcodePlus.get
    } else {
      prefixBytes ++: opcode.get
    }
    
    val addressForm = explicitFormat(operand, operand2) getOrElse implicitFormat.getAddressingForm(operand, operand2, opEx)
    
    prefixAndOpcode ++: addressForm.getBytes
  }
}


