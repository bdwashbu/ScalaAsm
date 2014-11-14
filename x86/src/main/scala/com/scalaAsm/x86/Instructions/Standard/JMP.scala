package com.scalaAsm.x86
package Instructions
package Standard

object JMP extends OperandInstruction[OneOpcode]("JMP") with jmpLow
 
trait jmpLow {

  implicit object jmp2 extends JMP.OneOp[rm32, M] {
    def opcode = 0xFF /+ 4
  }
}
