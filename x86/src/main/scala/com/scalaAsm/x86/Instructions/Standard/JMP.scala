package com.scalaAsm.x86
package Instructions
package Standard

abstract class JMP_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, OneOpcode]("JMP")

object JMP_1 {

  implicit object jmp2 extends JMP_1[rm32, M] {
    def opcode = 0xFF /+ 4
  }
}