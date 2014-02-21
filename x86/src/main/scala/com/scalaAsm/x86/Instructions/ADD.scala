package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction2, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait ADD extends ModRM

trait ADD_2[-O1, -O2] extends ADD {
  def get(op1: O1, op2: O2): Instruction
}

object ADD {
  
  implicit object add1 extends ADD_2[r32, imm8] {
    def get(x: r32, y: imm8) = new Instruction {
      val operands = MI(x,y)
      val opcodeExtension = Some(0.toByte)
      val opcode = 0x83.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended2(operands, opcodeExtension.get))
      val opcode2 = None
     }
  }
  
  implicit object add2 extends ADD_2[r16, imm8] {
    def get(x: r16, y: imm8) = new Instruction {
      val operands = MI(x,y)
      val opcodeExtension = Some(0.toByte)
      val opcode = 0x83.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended2(operands, opcodeExtension.get))
      val opcode2 = None
     }
  }
}