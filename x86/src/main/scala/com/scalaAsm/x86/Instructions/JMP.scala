package com.scalaAsm.x86
package Instructions

trait JMP extends x86Instruction {
  val mnemonic = "JMP"
}

trait JMP_1[OpEn, -O1 <: Operand] extends OneOperandInstruction[OpEn, O1] with JMP 

object JMP {

  implicit object jmp2 extends JMP_1[M, rm32] {
    val opcode = 0xFF /+ 4
  }
}