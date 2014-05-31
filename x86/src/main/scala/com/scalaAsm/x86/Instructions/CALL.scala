package com.scalaAsm.x86
package Instructions

abstract class CALL extends x86Instruction {
  val mnemonic = "CALL"
}

trait CALL_1[-O1 <: Operand] extends CALL with OneOperandInstruction[O1]

trait CALLLow {
  implicit object call2 extends CALL_1[rm32] {
    val opcode = 0xFF /+ 2
    def opEn = M
  }
}

object CALL extends CALLLow {

  implicit object call3 extends CALL_1[rel32] {
    val opcode: OpcodeFormat = 0xE8
    def opEn = M
  }

  implicit object call1 extends CALL_1[rm16] {
    val opcode = 0xFF /+ 2
    def opEn = M
  }
}