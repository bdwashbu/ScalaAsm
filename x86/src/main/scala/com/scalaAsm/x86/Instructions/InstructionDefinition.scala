package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Constant
import Memory._
import com.scalaAsm.x86.OpcodeFormat

trait InstructionDefinition {
  val mnemonic: String
  
  trait _0 extends x86Instruction {
    val mnemonic = InstructionDefinition.this.mnemonic
    def get = ZeroMachineCode(new NoOperandFormat {}, opcode, mnemonic)
    def hasImplicitOperand: Boolean = false
  }

  trait _1[-O1] extends x86Instruction {
    val mnemonic = InstructionDefinition.this.mnemonic
    def hasImplicitOperand: Boolean = false
    val format: OneOperandFormat[O1]

    def apply[X <: O1](p1: X, prefix: Seq[Prefix]) = {
      val prefixBytes = format.getPrefix(prefix, p1).map(_.get).foldLeft(Array[Byte]()) { _ ++ _ }
      OneMachineCode(opcode, p1, prefixBytes, mnemonic, format)
    }
  }

  trait _2[-O1, -O2] extends x86Instruction {
    val mnemonic = InstructionDefinition.this.mnemonic
    def hasImplicitOperand: Boolean = false
    val format: TwoOperandFormat[O1, O2]

    def apply[X <: O1, Y <: O2](p1: X, p2: Y) = {
      val prefixBytes = format.getPrefix(prefix, p1, p2).map(_.get).foldLeft(Array[Byte]()) { _ ++ _ }
      TwoMachineCode(opcode, p1, p2, prefixBytes, mnemonic, format)
    }
  }
}