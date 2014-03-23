package com.scalaAsm.x86
package Instructions

abstract class CALL extends x86Instruction("CALL")

trait CALL_1[-O1] extends CALL with OneOperand[O1] with InstructionFormat

trait CALLLow {
  implicit object call2 extends CALL_1[rm32] {
    val opcode = 0xFF /+ 2
    def operands = M(x)
  }
}

object CALL extends CALLLow {

  def callNear[O1](p1: O1)(implicit ev: CALL_1[O1]): CALL_1[O1] = {
    ev.set(p1)
    ev
  }

  implicit object call3 extends CALL_1[rel32] {
    val opcode: Opcodes = 0xE8
    def operands = M(x)
  }

  implicit object call1 extends CALL_1[rm16] {
    val opcode = 0xFF /+ 2
    def operands = M(x)
  }
}