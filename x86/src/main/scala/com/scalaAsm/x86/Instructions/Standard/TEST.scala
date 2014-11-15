package com.scalaAsm.x86
package Instructions
package Standard

object TEST extends InstructionDefinition[OneOpcode]("TEST") with testLow
 
trait testLow {

  implicit object test1 extends TEST._2[r32, rm32, RM] {
    def opcode = 0x85
  }

  implicit object test2 extends TEST._2[rm32, imm32, MI] {
    def opcode = 0xF7 /+ 0
  }
}