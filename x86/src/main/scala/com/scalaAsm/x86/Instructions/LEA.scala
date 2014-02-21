package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait LEA extends ModRM

trait LEA_2[-O1, -O2] extends LEA {
  def get(p1: O1, p2: O2): Instruction
}

object LEA {

  implicit object lea1 extends LEA_2[r32, rm32] {
    def get(x: r32, y: rm32) = new Instruction {
      val operands = RM(x,y)
      val opcode = 0x8D.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm2(operands))
      val opcodeExtension = Some(0.toByte)
      val opcode2 = None
     }
  }
}