package com.scalaAsm.x86
package Instructions

trait CALL extends x86Instruction {
  val mnemonic = "CALL"
}

trait CALL_1[X, -O1 <: Operand] extends OneOperandInstruction[X, O1] with CALL

trait CALLLow {
  implicit object call2 extends CALL_1[M, rm32] {
    val opcode = 0xFF /+ 2
  }
}

object CALL extends CALLLow {

  implicit object call3 extends CALL_1[M, rel32] {
    val opcode: OpcodeFormat = 0xE8
  }

  implicit object call1 extends CALL_1[M, rm16] {
    val opcode = 0xFF /+ 2
  }
}