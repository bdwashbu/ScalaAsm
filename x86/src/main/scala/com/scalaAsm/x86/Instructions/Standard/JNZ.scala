package com.scalaAsm.x86
package Instructions
package Standard

object JNZ extends OperandInstruction[OneOpcode]("JNZ") with jnzLow
 
trait jnzLow {
  
  implicit object jnz1 extends JNZ.OneOp[imm8, I] {
      def opcode = 0x75
  }
}