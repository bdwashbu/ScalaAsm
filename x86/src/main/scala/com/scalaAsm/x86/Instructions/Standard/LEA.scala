package com.scalaAsm.x86
package Instructions
package Standard

trait LEA extends x86Instruction {
  val mnemonic = "LEA"
}

trait LEA_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode] with LEA

object LEA {

  implicit object lea1 extends LEA_2[r, rm, RM] {
      def opcode = 0x8D
  }
}