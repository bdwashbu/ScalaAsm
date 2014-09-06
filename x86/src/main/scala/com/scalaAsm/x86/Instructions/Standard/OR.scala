package com.scalaAsm.x86
package Instructions
package Standard

trait OR extends x86Instruction {
  val mnemonic = "OR"
}

trait OR_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode] with OR

object OR {
  
  implicit object or1 extends OR_2[rm8, imm8, MI] {
      def opcode = 0x80 /+ 1
  }
}