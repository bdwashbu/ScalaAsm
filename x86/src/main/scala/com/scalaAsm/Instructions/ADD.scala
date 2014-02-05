package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import com.scalaAsm.x86.AddressingFormSpecifier
import x86Registers._

trait ADD extends Instruction with Operands

trait ADD_2[-O1, -O2] extends ADD {
  def get(op1: O1, op2: O2): Instruction1
}

object ADD extends Instruction {
  implicit object add1 extends ADD_2[r32, imm8] { def get(x: r32, y: imm8) = {
    new Instruction1(
      opcode = 0x83.toByte,
	  addressingFormSpecifier = new AddressingFormSpecifier {
            val modRM: ModRMByte = ModRMExtended(SecondReg, 0.toByte, x)
		    val scaleIndexBase: Option[Byte] = None
		    val displacment: Option[Byte] = None
		    val immediate: Option[Byte] = Some(y.value)
      }
     )
  }
  }
}