package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, x86Instruction, OperandSize, Opcodes, TwoOpcodes, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._
import scala.annotation.implicitNotFound

class RDRAND extends x86Instruction("RDRAND")

trait RDRAND_1[-O1] extends RDRAND {
  def get(p1: O1): Instruction
}

object RDRAND {
  
  implicit object rdrand1 extends RDRAND_1[rm32] {
    def get(x: rm32) = new Instruction {
      val operands = M(x)
      val opcode = (0x0F, 0xC7) /+ 6
    }
  }
  
  implicit object rdrand2 extends RDRAND_1[rm16] {
    def get(x: rm16) = new Instruction {
      val operands = M(x)
       val opcode = (0x0F, 0xC7) /+ 6
     }
  }
}