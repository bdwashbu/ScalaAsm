package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands.EAX
import com.scalaAsm.x86.Operands.GeneralPurposeA
import com.scalaAsm.x86.Operands.RAX

object ADD extends OperandInstruction[OneOpcode]("ADD") with AddOptimization
  
trait AddLow {
  
  implicit object add1 extends ADD.TwoOps[rm32, imm8, MI] {
      def opcode = 0x83 /+ 0
  }
  
  implicit object add2 extends ADD.TwoOps[rm16, imm8, MI] {
      def opcode = 0x83 /+ 0
  }
  
  implicit object add3 extends ADD.TwoOps[r32, rm32, RM] {
      def opcode = 0x03
  }
}

trait AddMedium extends AddLow {
  implicit object add4 extends ADD.TwoOps[GeneralPurposeA[_], imm32, I2] { // convenience opcode, 1 byte shorter (no modRM byte)
      def opcode = 0x05
  }
  
  implicit object add5 extends ADD.TwoOps[GeneralPurposeA[_], imm8, I2] { // convenience opcode, 1 byte shorter (no modRM byte)
      def opcode = 0x04
  }
}

trait AddOptimization extends AddMedium {
  implicit object add6 extends ADD.TwoOps[RAX, imm32, I2] { // convenience opcode, 1 byte shorter (no modRM byte)
      def opcode = 0x05
      override def prefix = REX.W(true)
  }
}
