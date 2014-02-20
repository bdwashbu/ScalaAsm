package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._

trait SHR extends ModRM

trait SHR_2[-O1, -O2] extends SHR {
  def get(p1: O1, p2: O2): Instruction
}

object SHR {
  
  abstract class MI[M <: ModRM.rm, I <: Immediate](op1: M, op2: I) extends Instruction2[M,I] {
     val operand1 = op1
     val operand2 = op2
     val opcode2 = None
  }
  
  implicit object shr1 extends SHR_2[r32, imm8] {
    def get(x: r32, y: imm8) = new MI(x,y) {
      val opcode = 0xC1.toByte
      val opcodeExtension = Some(5.toByte)
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended2(this))
     }
  }
}