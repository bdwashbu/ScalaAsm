package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._

trait TEST extends ModRM

trait TEST_2[-O1, -O2] extends TEST {
  def get(p1: O1, p2: O2): Instruction
}

object TEST {
  
  implicit object test1 extends TEST_2[r32, rm32] {
    def get(x: r32, y: rm32) = new Instruction {
      val operands = RM(x,y)
      val opcode = OneOpcode(0x85.toByte)
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm2(operands))
      val opcodeExtension = None
     }
  }
  
  implicit object test2 extends TEST_2[r32, imm32] {
    def get(x: r32, y: imm32) = new Instruction {
      val operands = MI(x,y)
      val opcodeExtension = Some(0.toByte)
      val opcode = OneOpcode(0xF7.toByte)
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended2(operands, opcodeExtension.get))
     }
  }
}