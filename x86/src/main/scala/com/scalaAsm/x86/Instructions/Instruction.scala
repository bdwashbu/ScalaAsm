package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Instructions._

trait OneOperand[X <: InstructionDefinition] {
  def apply[O1](p1: O1)(implicit ev: X#OneOp[O1]): OneMachineCode[O1] = ev(p1, ev.prefix)
}

trait TwoOperands[X <: InstructionDefinition] {
  def apply[O1, O2](p1: O1, p2: O2)(implicit ev: X#TwoOp[O1, O2]): TwoMachineCode[O1, O2] = ev(p1, p2)
}

trait ZeroOperands[X <: InstructionDefinition] {
  def apply(ignored: Unit)(implicit ev: X#NoOp): ZeroMachineCode = ev.get
}


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
  format: OneOperandFormat[O1]) extends InstructionResult {

  override def toString = {
    val formattedMnemonic = mnemonic.head + mnemonic.tail.toLowerCase()
    formattedMnemonic + " " + operand.toString
  }

  def apply: Array[Byte] = {
    val prefixAndOpcode = if (opcode.isInstanceOf[OpcodeWithReg] && operand.isInstanceOf[reg]) { // this is hacky as hell!
      val opcodePlus = opcode.asInstanceOf[OpcodeWithReg]
      opcodePlus.reg = operand.asInstanceOf[reg]
      prefixBytes ++: opcodePlus.get
    } else {
      prefixBytes ++: opcode.get
    }
    
    val addressForm = format.getAddressingForm(operand)
    
    prefixAndOpcode ++: addressForm.getBytes
  }
}

case class TwoMachineCode[O1, O2] (
  opcode: OpcodeFormat,
  operand: O1,
  operand2: O2,
  prefixBytes: Array[Byte],
  mnemonic: String,
  format: TwoOperandFormat[O1, O2]) extends InstructionResult {

  override def toString = {
    val formattedMnemonic = mnemonic.head + mnemonic.tail.toLowerCase()
      formattedMnemonic + " " + operand.toString + ", " + operand2.toString
    }
  
  def apply: Array[Byte] = {

    val prefixAndOpcode = if (opcode.isInstanceOf[OpcodeWithReg] && operand.isInstanceOf[reg]) { // this is hacky as hell!
      val opcodePlus = opcode.asInstanceOf[OpcodeWithReg]
      opcodePlus.reg = operand.asInstanceOf[reg]
      prefixBytes ++: opcodePlus.get
    } else {
      prefixBytes ++: opcode.get
    }
    
    val addressForm = format.getAddressingForm(operand, operand2)
    
    prefixAndOpcode ++: addressForm.getBytes
  }
}


