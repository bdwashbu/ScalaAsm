package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._

trait TEST extends ModRM

trait TEST_2[-O1, -O2] extends TEST {
  def get(p1: O1, p2: O2): Instruction
}

object TEST {
  
  abstract class MR[M <: ModRM.rm, R <: ModRM.reg](op1: M, op2: R) extends Instruction2[M,R] {
    val operand1 = op1
    val operand2 = op2
    val opcodeExtension = None
    val opcode2 = None
  }
  
  abstract class MI[R <: ModRM.rm, I <: Immediate](op1: R, op2: I) extends Instruction2[R,I] {
     val opcodeExtension = Some(0.toByte)
     val operand1 = op1
     val operand2 = op2
     val opcode2 = None
  }
  
  implicit object test1 extends TEST_2[r32, rm32] {
    def get(x: r32, y: rm32) = new RM(x,y) {
      val opcode = 0x85.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm2(this))
     }
  }
  
  implicit object test2 extends TEST_2[r32, imm32] {
    def get(x: r32, y: imm32) = new MI(x,y) {
      val opcode = 0xF7.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended2(this))
     }
  }
}