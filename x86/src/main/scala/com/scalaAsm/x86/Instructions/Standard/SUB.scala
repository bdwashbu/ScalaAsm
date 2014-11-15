package com.scalaAsm.x86
package Instructions
package Standard

object SUB extends InstructionDefinition[OneOpcode]("SUB") with subLow
 
trait subLow {

  implicit object sub1 extends SUB._2[rm32, imm8, MI] {
    def opcode = 0x83 /+ 5
  }

  implicit object sub3 extends SUB._2[rm64, imm8, MI] {
    def opcode = 0x83 /+ 5
    override def prefix = REX.W(true)
  }

  implicit object sub2 extends SUB._2[r32, rm32, RM] {
    def opcode = 0x2B
  }
}
