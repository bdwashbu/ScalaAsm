package com.scalaAsm.x86
package Instructions
package Standard

trait JMP extends x86Instruction {
  val mnemonic = "JMP"
}

trait JMP_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, OneOpcode] with JMP 

object JMP {

  implicit object jmp2 extends JMP_1[rm32, M] {
    val opcode = 0xFF /+ 4
  }
}