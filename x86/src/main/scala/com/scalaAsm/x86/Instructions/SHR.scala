package com.scalaAsm.x86
package Instructions

trait SHR extends x86Instruction {
  val mnemonic = "SHR"
}

trait SHR_2[OpEn, -O1 <: Operand, -O2 <: Operand] extends TwoOperandInstruction[OpEn, O1,O2] with SHR

object SHR extends Formats2 {
  
  implicit object shr1 extends SHR_2[MI, rm32, imm8] {
      val opcode = 0xC1 /+ 5
  }
}