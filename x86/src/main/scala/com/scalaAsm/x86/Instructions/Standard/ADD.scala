package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands.EAX
import com.scalaAsm.x86.Operands.GeneralPurposeA
import com.scalaAsm.x86.Operands.RAX

abstract class ADD_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1,O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode]("ADD")

trait AddLow {
  
  implicit object add1 extends ADD_2[rm32, imm8, MI] {
      def opcode = 0x83 /+ 0
  }
  
  implicit object add2 extends ADD_2[rm16, imm8, MI] {
      def opcode = 0x83 /+ 0
  }
  
  implicit object add3 extends ADD_2[r32, rm32, RM] {
      def opcode = 0x03
  }
}

trait AddMedium extends AddLow {
  implicit object add4 extends ADD_2[GeneralPurposeA[_ <: OperandSize], imm32, I2] { // convenience opcode, 1 byte shorter (no modRM byte)
      def opcode = 0x05
  }
  
  implicit object add5 extends ADD_2[GeneralPurposeA[_ <: OperandSize], imm8, I2] { // convenience opcode, 1 byte shorter (no modRM byte)
      def opcode = 0x04
  }
}

object ADD_2 extends AddMedium {
  implicit object add6 extends ADD_2[RAX, imm32, I2] { // convenience opcode, 1 byte shorter (no modRM byte)
      def opcode = 0x05
      override def prefix = REX.W(true)
  }
}