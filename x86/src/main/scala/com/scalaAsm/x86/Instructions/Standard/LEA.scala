package com.scalaAsm.x86
package Instructions
package Standard

object LEA extends InstructionDefinition[OneOpcode]("LEA") with leaHigh
 
trait leaLow {
  implicit object lea1 extends LEA._2[reg, rm, RM] {
      def opcode = 0x8D
  }
}

trait leaHigh extends leaLow {

  implicit object lea2 extends LEA._2[r64, rm, RM] {
      def opcode = 0x8D
      override def prefix = REX.W(true)
  }
}
