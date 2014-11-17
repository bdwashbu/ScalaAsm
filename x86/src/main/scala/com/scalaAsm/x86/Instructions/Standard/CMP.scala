package com.scalaAsm.x86
package Instructions
package Standard

object CMP extends InstructionDefinition[OneOpcode]("CMP") with cmpLow
  
trait cmpLow {
  implicit object cmp1 extends CMP._2[rm32, imm8, MI] {
    def opcode = 0x83 /+ 7
  }
}