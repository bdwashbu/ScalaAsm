package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.x86Registers._
import com.scalaAsm.x86.AddressingFormSpecifier

trait POP extends ModRM

trait POP_1[-O1] extends POP {
  def get(p1: O1): Instruction
}



object POP {
  
  implicit object pop1 extends POP_1[r32] {
    def get(x: r32) = new Instruction {
      val operands = O(x)
      val opcode = OneOpcode((0x58 + x.ID).toByte)
	  val opcodeExtension = None
	  val modRM: Option[AddressingFormSpecifier] = None
    }
  }
  
  implicit object pop2 extends POP_1[DS] {
    def get(x: DS) = new Instruction {
      val operands = new OneOperand[DS](x) {}
      val opcodeExtension = None
      val opcode = OneOpcode(0x1F.toByte)
      val modRM: Option[AddressingFormSpecifier] = None
     }
  }
}