package com.scalaAsm.x86
package Instructions

abstract class LEA extends x86Instruction("LEA")

trait LEA_2[-O1, -O2] extends LEA with TwoOperandInstruction[O1,O2] with InstructionFormat

object LEA {

  implicit object lea1 extends LEA_2[r32, rm32] {
      def operands = RM()
      val opcode = 0x8D /+ 0
  }
}