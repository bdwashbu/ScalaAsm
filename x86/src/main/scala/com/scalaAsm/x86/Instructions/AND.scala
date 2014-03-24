package com.scalaAsm.x86
package Instructions

abstract class AND extends x86Instruction("AND")

trait AND_2[-O1, -O2] extends AND with TwoOperands[O1,O2] with InstructionFormat

object AND {
  
  implicit object and1 extends AND_2[r32, rm32] {
      def operands = RM(op1,op2)
      val opcode: OpcodeFormat = 0x23
  }
}