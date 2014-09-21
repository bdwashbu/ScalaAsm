package com.scalaAsm.x86
package Instructions
package Standard

abstract class LEA_2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]] extends TwoOperandInstruction[O1, O2, OpEn, OneOpcode]("LEA")

trait leaLow {
  implicit object lea1 extends LEA_2[r, rm, RM] {
      def opcode = 0x8D
  }
}

object LEA_2 extends leaLow {

  implicit object lea2 extends LEA_2[r64, rm, RM] {
      def opcode = 0x8D
      override def prefix = REX.W(true)
  }
  
  
}