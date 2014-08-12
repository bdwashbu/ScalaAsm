package com.scalaAsm.x86
package Instructions
package Standard

trait SBB extends x86Instruction {
  val mnemonic = "SBB"
}

trait SBB_2[OpEn, -O1, -O2] extends TwoOperandInstruction[OpEn, O1,O2] with SBB

object SBB {
  
  implicit object sbb1 extends SBB_2[RM, r32, rm32] {
      val opcode: OpcodeFormat = 0x1B
  }
}