package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands.UniformByteRegister

object AND extends OperandInstruction[OneOpcode]("AND") with andHigh
  
trait ANDLow {
 
  implicit object and1 extends AND.TwoOps[r32, rm32, RM] {
      def opcode = 0x23
  } 
  
  implicit object and3 extends AND.TwoOps[rm8, imm8, MI] {
      def opcode = 0x80 /+ 4
  }
}

trait andHigh extends ANDLow {

  implicit object and2 extends AND.TwoOps[r16, rm16, RM] {
      def opcode = 0x23
  }
  
  implicit object and13 extends AND.TwoOps[UniformByteRegister[_8], imm8, MI] {
    def opcode = 0x80 /+ 4
    override def prefix = REX(false, false, false, false)
  }
  
  implicit object and4 extends AND.TwoOps[rm16, imm16, MI] {
      def opcode = 0x81 /+ 4
  }
  
  implicit object and5 extends AND.TwoOps[rm32, imm32, MI] {
      def opcode = 0x81 /+ 4
  }
  
  implicit object and6 extends AND.TwoOps[rm16, imm8, MI] {
      def opcode = 0x83 /+ 4
  }
  
  implicit object and7 extends AND.TwoOps[rm32, imm8, MI] {
      def opcode = 0x83 /+ 4
  }
  
  implicit object and9 extends AND.TwoOps[rm16, r16, MR] {
      def opcode = 0x21
  }
  
  implicit object and10 extends AND.TwoOps[rm32, r32, MR] {
      def opcode = 0x21
  }
  
  implicit object and11 extends AND.TwoOps[r8, rm8, RM] {
      def opcode = 0x22
  }
}
