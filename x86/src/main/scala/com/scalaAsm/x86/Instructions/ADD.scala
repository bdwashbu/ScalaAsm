package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import com.scalaAsm.x86.AddressingFormSpecifier
import x86Registers._

trait ADD extends ModRM with Operands

trait ADD_2[-O1, -O2] extends ADD {
  def get(op1: O1, op2: O2): Instruction1  
}

trait MI[X <: OperandSize] extends ADD_2[ModRM.reg[X], imm8]
trait MR[X <: OperandSize] extends ADD_2[ModRM.rm[X], ModRM.reg[X]]

object ADD extends Instruction {
  implicit object add1 extends MI[DwordOperand] {
    def get(x: r32, y: imm8) = {
	    Instruction.newInst2( 
	      operand1 = x,
	      operand2 = y,
	      opcode = 0x83.toByte,
	      opcodeExtension = 0.toByte
	     )
     }
  }
}