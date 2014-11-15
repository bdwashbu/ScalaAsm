package com.scalaAsm.x86
package Instructions
package Standard

object SHR extends InstructionDefinition[OneOpcode]("SHR") with shrLow
 
trait shrLow {
  
  implicit object shr1 extends SHR._2[rm32, imm8, MI] {
      def opcode = 0xC1 /+ 5
  }
}