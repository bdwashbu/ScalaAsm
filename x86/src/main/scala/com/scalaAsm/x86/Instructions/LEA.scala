package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait LEA extends ModRM

trait LEA_2[-O1, -O2] extends LEA {
  def get(p1: O1, p2: O2): Instruction
}

abstract class RM[R <: ModRM.reg, M <: ModRM.rm](op1: R, op2: M) extends Instruction2[R,M] {
  val opcodeExtension = Some(0.toByte)
  val operand1 = op1
  val operand2 = op2
  val opcode2 = None
}

object LEA {

  implicit object lea1 extends LEA_2[r32, rm32] {
    def get(x: r32, y: rm32) = new RM(x,y) {
      val opcode = 0x8D.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm2(this))
     }
  }
}