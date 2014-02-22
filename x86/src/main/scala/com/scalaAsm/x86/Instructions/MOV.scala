package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.utils.Endian

trait MOV

trait MOV_2[-O1, -O2] extends MOV {
  def get(op1: O1, op2: O2): Instruction
}

trait MOVLow {
  
 implicit object mov1 extends MOV_2[rm32, r32] {
    def get(x: rm32, y: r32) = new Instruction {
      val operands = MR(x,y)
      val opcode = OneOpcode(0x89)
     }
  }
}

object MOV extends MOVLow {
 
  implicit object mov3 extends MOV_2[r32, rm32] {
    def get(x: r32, y: rm32) = new Instruction {
      val operands = RM(x,y)
      val opcode = OneOpcode(0x8B)
     }
  }
  
  implicit object mov9 extends MOV_2[r16, rm16] {
    def get(x: r16, y: rm16) = new Instruction {
      val operands = RM(x,y)
      val opcode = OneOpcode(0x8B)
     }
  }
  
  implicit object mov7 extends MOV_2[r16, imm16] {
    def get(x: r16, y: imm16) = new Instruction {
      val operands = I(y) // should be OI
      val opcode = OneOpcode(0xB8 + x.reg.ID)
     }
  }
  
  implicit object mov8 extends MOV_2[r8, imm8] {
    def get(x: r8, y: imm8) = new Instruction {
      val operands = I(y)
      val opcode = OneOpcode(0xB0 + x.reg.ID)
     }
  }
  
   implicit object mov6 extends MOV_2[r32, imm32] {
    def get(x: r32, y: imm32) = new Instruction {
      val operands = I(y)
      val opcode = OneOpcode(0xB8 + x.reg.ID)
     }
  }
}