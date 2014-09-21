package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands.UniformByteRegister

trait AND extends x86Instruction {
  val mnemonic = "AND"
}

trait AND_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1,O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode] with AND

trait ANDLow {
 
  implicit object and1 extends AND_2[r32, rm32, RM] {
      def opcode = 0x23
  } 
  
  implicit object and3 extends AND_2[rm8, imm8, MI] {
      def opcode = 0x80 /+ 4
  }
}

object AND extends ANDLow {

  implicit object and2 extends AND_2[r16, rm16, RM] {
      def opcode = 0x23
  }
  
  implicit object and13 extends AND_2[UniformByteRegister[_8], imm8, MI] {
    def opcode = 0x80 /+ 4
    override def prefix = REX(false, false, false, false)
  }
  
  implicit object and4 extends AND_2[rm16, imm16, MI] {
      def opcode = 0x81 /+ 4
  }
  
  implicit object and5 extends AND_2[rm32, imm32, MI] {
      def opcode = 0x81 /+ 4
  }
  
  implicit object and6 extends AND_2[rm16, imm8, MI] {
      def opcode = 0x83 /+ 4
  }
  
  implicit object and7 extends AND_2[rm32, imm8, MI] {
      def opcode = 0x83 /+ 4
  }
  
  implicit object and9 extends AND_2[rm16, r16, MR] {
      def opcode = 0x21
  }
  
  implicit object and10 extends AND_2[rm32, r32, MR] {
      def opcode = 0x21
  }
  
  implicit object and11 extends AND_2[r8, rm8, RM] {
      def opcode = 0x22
  }
}