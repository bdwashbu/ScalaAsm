package com.scalaAsm.x86
package Instructions
package Standard

object JNZ extends InstructionDefinition[OneOpcode]("JNZ") with jnzLow
 
trait jnzLow {
  
  implicit object jnz1 extends JNZ._1[imm8] {
      def opcode = 0x75
  }
}