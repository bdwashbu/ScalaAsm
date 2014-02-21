package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._

trait SBB extends ModRM

trait SBB_2[-O1, -O2] extends SBB {
  def get(x: O1, y: O2): Instruction
}

object SBB {
  
  implicit object sbb1 extends SBB_2[r32, rm32] {
    def get(x: r32, y: rm32) = new Instruction {
      val operands = RM(x,y)
      val opcode = 0x1B.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm2(operands))
    val opcodeExtension = None
    val opcode2 = None
     }
  }
}