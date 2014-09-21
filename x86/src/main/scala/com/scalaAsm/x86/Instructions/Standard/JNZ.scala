package com.scalaAsm.x86
package Instructions
package Standard

abstract class JNZ_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, OneOpcode]("JNZ")

object JNZ_1 {
  
  implicit object jnz1 extends JNZ_1[imm8, I] {
      def opcode = 0x75
  }
}