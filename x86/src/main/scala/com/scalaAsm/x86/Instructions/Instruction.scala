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

object NoAddressingForm extends InstructionFormat(addressingForm = NoModRM(), immediate = None)

case class ZeroMachineCode(format: ResolvedZeroOperand, opcode: OpcodeFormat, mnemonic: String) extends InstructionResult {

  def getBytes: Array[Byte] = {
    format.getPrefix ++: opcode.get
  }
}

case class OneMachineCode[O1] (
  opcode: OpcodeFormat,
  operand: Operand[O1],
  prefixBytes: Array[Byte],
  mnemonic: String,
  explicitFormat: (O1) => Option[InstructionFormat],
  implicitFormat: OneOperandFormat[O1]) extends InstructionResult {

  override def toString = {
    val formattedMnemonic = mnemonic.head + mnemonic.tail.toLowerCase()
    formattedMnemonic + " " + operand.toString
  }

  def getBytes: Array[Byte] = {
    val opEx: Byte = if (!opcode.opcodeExtension.isEmpty) opcode.opcodeExtension.get else -1
    
    val prefixAndOpcode = if (opcode.isInstanceOf[OpcodeWithReg] && operand().isInstanceOf[reg]) { // this is hacky as hell!
      val opcodePlus = opcode.asInstanceOf[OpcodeWithReg]
      opcodePlus.reg = operand().asInstanceOf[reg]
      prefixBytes ++: opcodePlus.get
    } else {
      prefixBytes ++: opcode.get
    }
    
    val addressForm = explicitFormat(operand()) getOrElse implicitFormat.getAddressingForm(operand(), opEx)
    
    prefixAndOpcode ++: addressForm.getBytes
  }
}

case class TwoMachineCode[O1, O2] (
  opcode: OpcodeFormat,
  operand: Operand[O1],
  operand2: Operand[O2],
  prefixBytes: Array[Byte],
  mnemonic: String,
  explicitFormat: (O1, O2) => Option[InstructionFormat],
  implicitFormat: TwoOperandFormat[O1, O2]) extends InstructionResult {

  override def toString = {
    val formattedMnemonic = mnemonic.head + mnemonic.tail.toLowerCase()
      formattedMnemonic + " " + operand.toString + ", " + operand2.toString
    }
  
  def getBytes: Array[Byte] = {
    val opEx: Byte = if (!opcode.opcodeExtension.isEmpty) opcode.opcodeExtension.get else -1
    
    val prefixAndOpcode = if (opcode.isInstanceOf[OpcodeWithReg] && operand().isInstanceOf[reg]) { // this is hacky as hell!
      val opcodePlus = opcode.asInstanceOf[OpcodeWithReg]
      opcodePlus.reg = operand().asInstanceOf[reg]
      prefixBytes ++: opcodePlus.get
    } else {
      prefixBytes ++: opcode.get
    }
    
    val addressForm = explicitFormat(operand(), operand2()) getOrElse implicitFormat.getAddressingForm(operand(), operand2(), opEx)
    
    prefixAndOpcode ++: addressForm.getBytes
  }
}


