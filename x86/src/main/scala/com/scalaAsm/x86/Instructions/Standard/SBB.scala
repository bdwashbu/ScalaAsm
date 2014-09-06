package com.scalaAsm.x86
package Instructions
package Standard

trait SBB extends x86Instruction {
  val mnemonic = "SBB"
}

trait SBB_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode] with SBB

object SBB {
  
  implicit object sbb1 extends SBB_2[r32, rm32, RM] {
      val opcode = OneOpcode(0x1B)
  }
}