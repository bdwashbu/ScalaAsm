package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Constant
import Memory._
import com.scalaAsm.x86.OpcodeFormat

abstract class InstructionDefinition[Opcode <: OpcodeFormat](val mnemonic: String) {

  abstract class _0 extends x86Instruction {
    def opcode: Opcode
    val mnemonic = InstructionDefinition.this.mnemonic
    def get = ZeroMachineCode(new NoOperandFormat {}, opcode, mnemonic)
    def hasImplicateOperand: Boolean = false
  }

  abstract class _1[-O1] extends x86Instruction {
    val mnemonic = InstructionDefinition.this.mnemonic
    def opcode: Opcode
    def hasImplicateOperand: Boolean = false
    def explicitFormat: Option[InstructionFormat] = None

    def apply[X <: O1](p1: Operand[X], implicitFormat: OneOperandFormat[X], prefix: Seq[Prefix]) = {
      val opEx = if (!opcode.opcodeExtension.isEmpty) opcode.opcodeExtension.get else -1

      val addressForm = explicitFormat getOrElse implicitFormat.getAddressingForm(p1(), opEx)

      if (opcode.isInstanceOf[OpcodeWithReg] && p1().isInstanceOf[reg]) { // this is hacky as hell!
        val opcodePlus = opcode.asInstanceOf[OpcodeWithReg]
        opcodePlus.reg = p1().asInstanceOf[reg]
        val opcodeBytes = implicitFormat.getPrefix(prefix).map(_.get).foldLeft(Array[Byte]()) { _ ++ _ } ++: opcodePlus.get
        OneMachineCode(p1, addressForm.getBytes, opcodeBytes, mnemonic, opEx)
      } else {
        val opcodeBytes = implicitFormat.getPrefix(prefix).map(_.get).foldLeft(Array[Byte]()) { _ ++ _ } ++: opcode.get
        OneMachineCode(p1, addressForm.getBytes, opcodeBytes, mnemonic, opEx)
      }
    }
  }

  abstract class _2[-O1, -O2] extends x86Instruction {
    val mnemonic = InstructionDefinition.this.mnemonic
    def opcode: Opcode
    def hasImplicateOperand: Boolean = false
    def explicitFormat(p1: O1, p2: O2): Option[InstructionFormat] = None

    def apply[X <: O1, Y <: O2](p1: Operand[X], p2: Operand[Y], implicitFormat: TwoOperandFormat[X, Y]) = {
      val prefixBytes = implicitFormat.getPrefix(prefix).map(_.get).foldLeft(Array[Byte]()) { _ ++ _ }
      TwoMachineCode(opcode, p1, p2, prefixBytes, mnemonic, explicitFormat, implicitFormat)
    }
  }
}