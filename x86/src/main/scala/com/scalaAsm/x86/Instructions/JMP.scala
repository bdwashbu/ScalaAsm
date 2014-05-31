package com.scalaAsm.x86
package Instructions

trait JMP extends x86Instruction {
  val mnemonic = "JMP"
}

trait JMP_1[-O1 <: Operand] extends JMP with OneOperandInstruction[O1]

object JMP {

  implicit object jmp2 extends JMP_1[rm32] {
    val opcode = 0xFF /+ 4
    def opEn = M
  }
}