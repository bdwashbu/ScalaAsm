package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._
import scala.annotation.implicitNotFound

trait PUSH extends x86Instruction {
  val mnemonic = "PUSH"
}

@implicitNotFound(msg = "Cannot find PUSH implementation for ${O1}")
trait PUSH_1[OpEn, -O1 <: Operand] extends OneOperandInstruction[OpEn, O1] with PUSH

trait POWLow extends Formats {
    
  implicit object LowPush extends PUSH_1[M, rm] {
      val opcode = 0xFF /+ 6
  }
}

object PUSH extends POWLow {

  implicit object push1 extends PUSH_1[O, r64] {
      val opcode = OpcodePlusRd(0x50)
  }
  
  implicit object push2 extends PUSH_1[O, r32] {
      val opcode = OpcodePlusRd(0x50)
  }
  
  implicit object push3 extends PUSH_1[O, r16] {
      val opcode = OpcodePlusRd(0x50)
  }
  
  implicit object push4 extends PUSH_1[I, imm8] {
      val opcode: OpcodeFormat = 0x6A
  }
  
  implicit object push5 extends PUSH_1[I, imm16] {
      val opcode: OpcodeFormat = 0x68
  }
  
  implicit object push6 extends PUSH_1[I, imm32] {
      val opcode: OpcodeFormat = 0x68
  }
  
  implicit object push7 extends PUSH_1[CS, CS] {
      val opcode: OpcodeFormat = 0x0E
  }
}