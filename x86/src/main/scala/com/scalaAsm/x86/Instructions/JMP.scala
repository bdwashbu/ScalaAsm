package com.scalaAsm.x86
package Instructions

abstract class JMP extends x86Instruction("JMP")

trait JMP_1[-O1] extends JMP with OneOperandInstruction[O1] with InstructionFormat

object JMP {

  def jmp[O1](p1: O1)(implicit ev: JMP_1[O1]): MachineCode = {
    ev(p1)
  }

  implicit object jmp2 extends JMP_1[rm32] {
    val opcode = 0xFF /+ 4
    def operands = M()
  }
}