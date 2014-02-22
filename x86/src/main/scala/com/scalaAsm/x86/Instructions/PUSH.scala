package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, x86Instruction, OperandSize, Opcodes, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._
import scala.annotation.implicitNotFound

class PUSH extends x86Instruction("PUSH")

@implicitNotFound(msg = "Cannot find PUSH implementation for ${O1}")
trait PUSH_1[-O1] extends PUSH {
  def get(op1: O1): Instruction
}

trait POWLow {
    
  implicit object push6 extends PUSH_1[rm32] {
    def get(x: rm32) = new Instruction {
      val operands = M(x)
      val opcode = 0xFF /+ 6
    }
  }
}

object PUSH extends POWLow {
  
  implicit object push1 extends PUSH_1[r32] {
    def get(x: r32) = new Instruction {
      val operands = O(x)
      val opcode: Opcodes = 0x50 + x.ID
    }
  }
  
  implicit object push8 extends PUSH_1[r16] {
    def get(x: r16) = new Instruction {
      val operands = O(x)
      val opcode: Opcodes = 0x50 + x.ID
    }
  }
  
  implicit object push4 extends PUSH_1[imm8] {
    def get(x: imm8) = new Instruction {
      val operands = I[imm8](x)
      val opcode: Opcodes = 0x6A
    }
  }
  
  implicit object push5 extends PUSH_1[imm16] {
    def get(x: imm16) = new Instruction {
      val operands = I[imm16](x)
      val opcode: Opcodes = 0x68
    }
  }
  
  implicit object push7 extends PUSH_1[CS] {
    def get(x: CS) = new Instruction {
      val operands = new OneOperand[CS](x) {def getAddressingForm = null}
      val opcode: Opcodes = 0x0E
    }
  }
}