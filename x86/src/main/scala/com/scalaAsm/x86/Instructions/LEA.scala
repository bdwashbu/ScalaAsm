package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import x86Registers._
import Addressing._
import com.scalaAsm.utils.Endian

trait LEA extends ModRM with Operands

trait LEA_2[-O1, -O2] extends LEA {
  def get(p1: O1, p2: O2): Instruction
}

abstract class RM[X <: OperandSize](op1: ModRM.reg[X], op2: ModRM.rm[X]) extends Instruction2[ModRM.reg[X], ModRM.rm[X]] {
  val opcodeExtension = Some(0.toByte)
  val operand1 = op1
  val operand2 = op2
}

object LEA {

  implicit object lea1 extends LEA_2[r32, rm32] {
    def get(x: r32, y: rm32) = new RM[DwordOperand](x,y) {
      val opcode = 0x8D.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm2(this))
     }
  }
}