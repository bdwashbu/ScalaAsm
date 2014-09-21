package com.scalaAsm.x86
package Instructions
package Standard

abstract class SHR_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode]("SHR")

object SHR_2 {
  
  implicit object shr1 extends SHR_2[rm32, imm8, MI] {
      def opcode = 0xC1 /+ 5
  }
}