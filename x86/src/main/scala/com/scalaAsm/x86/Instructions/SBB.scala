package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._

trait SBB extends ModRM

trait SBB_2[-O1, -O2] extends SBB {
  def get(x: O1, y: O2): Instruction
}

object SBB {
  
  abstract class RM[X <: OperandSize](op1: ModRM.reg[X], op2: ModRM.rm[X]) extends Instruction2[ModRM.reg[X], ModRM.rm[X]] {
    val operand1 = op1
    val operand2 = op2
    val opcodeExtension = None
  }
  
  implicit object sbb1 extends SBB_2[r32, rm32] {
    def get(x: r32, y: rm32) = new RM[DwordOperand](x,y) {
      val opcode = 0x1B.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm2(this))
     }
  }
  
  //implicit object sbb1 extends SBB_2[r32, rm32] { def get(x: r32, y: rm32) = 0x1B.toByte +: modRM2(x, y).getBytes }
}