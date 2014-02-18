package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait NOT extends ModRM

trait NOT_1[-O1] extends NOT {
  def get(p: O1): Instruction
}

object NOT {
  
  abstract class M[X <: OperandSize](op1: ModRM.rm[X]) extends Instruction1[ModRM.rm[X]] {
     val opcodeExtension = Some(2.toByte)
     val operand1 = op1
  }
  
  implicit object not1 extends NOT_1[rm32] {
    def get(x: rm32) = new M[DwordOperand](x) {
      val opcode = 0xF7.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended1(this))
     }
  }
  
  //implicit object not1 extends NOT_M[rm32] { def get(x: rm32) = 0xF7.toByte +: modRMExtended(x, extensionCode = 2).getBytes }
}