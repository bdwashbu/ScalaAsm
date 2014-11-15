package com.scalaAsm.x86
package Instructions
package Standard

object JMP extends InstructionDefinition[OneOpcode]("JMP") with jmpLow
 
trait jmpLow {

  implicit object jmp2 extends JMP._1[rm32, M] {
    def opcode = 0xFF /+ 4
  }
}
