package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.utils.Endian

trait MOV

trait MOV_2[-O1, -O2] extends MOV {
  def get(op1: O1, op2: O2): Array[Byte]
}

trait MOVLow extends ModRM {
   implicit object mov1 extends MOV_2[rm32, r32] { def get(x: rm32, y: r32) = 0x89.toByte +: modRM2(x, y).getBytes }  
}

object MOV extends MOVLow {
 
implicit object mov3 extends MOV_2[r32, rm32] { def get(x: r32, y: rm32) = 0x8B.toByte +: modRM2(x, y).getBytes }  
  implicit object mov9 extends MOV_2[r16, rm16] { def get(x: r16, y: rm16) = 0x8B.toByte +: modRM2(x, y).getBytes }

  implicit object mov7 extends MOV_2[r16, imm16] { def get(x: r16, y: imm16) = Array[Byte]((0xB8 + x.ID).toByte, (y.value & 0x00FF).toByte, ((y.value & 0xFF00) >> 8).toByte) }
  implicit object mov8 extends MOV_2[r8, imm8] { def get(x: r8, y: imm8) = Array[Byte]((0xB0 + x.ID).toByte, y.value) }

  implicit object mov6 extends MOV_2[r32, imm32] { def get(x: r32, y: imm32) = (0xB8 + x.ID).toByte +: Endian.swap(y.value) }
}