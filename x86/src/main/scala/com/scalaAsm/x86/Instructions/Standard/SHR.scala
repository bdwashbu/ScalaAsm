package com.scalaAsm.x86
package Instructions
package Standard

object SHR extends OperandInstruction[OneOpcode]("SHR") with shrLow
 
trait shrLow {
  
  implicit object shr1 extends SHR.TwoOps[rm32, imm8, MI] {
      def opcode = 0xC1 /+ 5
  }
}