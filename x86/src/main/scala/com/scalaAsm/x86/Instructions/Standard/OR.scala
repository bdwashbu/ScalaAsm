package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands.UniformByteRegister

trait OR extends x86Instruction {
  val mnemonic = "OR"
}

trait OR_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode] with OR

trait orLow {
 implicit object or1 extends OR_2[rm8, imm8, MI] {
      def opcode = 0x80 /+ 1
  } 
}

object OR {
  
  implicit object or2 extends OR_2[UniformByteRegister[_8], imm8, MI] {
    def opcode = 0x80 /+ 1
    override def prefix = REX(false, false, false, false)
  }
}