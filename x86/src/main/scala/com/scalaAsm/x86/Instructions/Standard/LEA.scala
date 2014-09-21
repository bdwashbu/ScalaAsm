package com.scalaAsm.x86
package Instructions
package Standard

trait LEA extends x86Instruction {
  val mnemonic = "LEA"
}

trait LEA_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode] with LEA

trait leaLow {
  implicit object lea1 extends LEA_2[r, rm, RM] {
      def opcode = 0x8D
  }
}

object LEA extends leaLow {

  implicit object lea2 extends LEA_2[r64, rm, RM] {
      def opcode = 0x8D
      override def prefix = REX.W(true)
  }
  
  
}