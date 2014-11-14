package com.scalaAsm.x86
package Instructions
package Standard

object JZ extends OperandInstruction[OneOpcode]("JZ") with jzLow
 
trait jzLow {
  implicit object jz1 extends JZ.OneOp[imm8, I] {
      def opcode = 0x74
  }
}
