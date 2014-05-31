package com.scalaAsm.x86
package Instructions

abstract class SHR extends x86Instruction {
  val mnemonic = "SHR"
}

trait SHR_2[-O1 <: Operand, -O2 <: Operand] extends SHR with TwoOperandInstruction[O1,O2]

object SHR {
  
  implicit object shr1 extends SHR_2[rm32, imm8] {
      def opEn = MI
      val opcode = 0xC1 /+ 5
  }
}