package com.scalaAsm.x86
package Instructions
package Standard

abstract class JZ_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, OneOpcode]("JZ")

object JZ_1 {
  
  implicit object jz1 extends JZ_1[imm8, I] {
      def opcode = 0x74
  }
}