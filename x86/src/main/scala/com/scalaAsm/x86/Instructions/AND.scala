package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._
import com.scalaAsm.x86.{ModRM, Instruction, OneOpcode, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait AND extends ModRM

trait AND_2[-O1, -O2] extends AND {
  def get(p1: O1, p2: O2): Instruction
}

object AND {
  
  implicit object and1 extends AND_2[r32, rm32] {
    def get(x: r32, y: rm32) = new Instruction {
      val operands = RM(x,y)
      val opcode = OneOpcode(0x23)
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm2(operands))
      val opcodeExtension = None
     }
  }
}