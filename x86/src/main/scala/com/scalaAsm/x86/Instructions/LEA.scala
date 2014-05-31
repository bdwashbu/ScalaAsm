package com.scalaAsm.x86
package Instructions

trait LEA extends x86Instruction {
  val mnemonic = "LEA"
}

trait LEA_2[-O1 <: Operand, -O2 <: Operand] extends LEA with TwoOperandInstruction[O1,O2]

object LEA {

  implicit object lea1 extends LEA_2[r, rm] {
      def opEn = RM
      val opcode: OpcodeFormat = 0x8D
  }
}