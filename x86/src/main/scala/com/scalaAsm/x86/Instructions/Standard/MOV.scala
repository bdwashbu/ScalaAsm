package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands.TwoOperandFormat

object MOV extends InstructionDefinition[OneOpcode]("MOV") with MOVHigh
 
trait MOVLow {

  implicit object mov1 extends MOV._2[rm32, r32, MR] {
    def opcode = 0x89
  }
}

trait MOVHigh extends MOVLow {

  implicit object mov3 extends MOV._2[r32, rm32, RM] {
    def opcode = 0x8B
  }

  implicit object mov9 extends MOV._2[r16, rm16, RM] {
    def opcode = 0x8B
  }

  implicit object mov7 extends MOV._2[r16, imm16, OI] {
    def opcode = 0xB8 + rw
  }

  implicit object mov8 extends MOV._2[r8, imm8, OI] {
    def opcode = 0xB0 + rb
  }

  implicit object mov6 extends MOV._2[r32, imm32, OI] {
    def opcode = 0xB8 + rd
  }

  implicit object mov10 extends MOV._2[r64, imm64, OI] {
    def opcode = 0xB8 + rd
    override def prefix = REX.W(true)
  }

  implicit object mov11 extends MOV._2[rm64, r64, MR] {
    def opcode = 0x89
    override def prefix = REX.W(true)
  }

  implicit object mov12 extends MOV._2[rm64, imm32, MI] {
    def opcode = 0xC7 /+ 0
    override def prefix = REX.W(true)
  }
}