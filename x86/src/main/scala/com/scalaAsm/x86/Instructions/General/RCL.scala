package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object RCL extends InstructionDefinition[OneOpcode]("RCL") with RCLImpl

// Rotate

trait RCLLow {
  implicit object RCL_0 extends RCL._1[rm16] {
    def opcode = 0xD1 /+ 2
    override def hasImplicitOperand = true
  }

  implicit object RCL_1 extends RCL._1[rm32] {
    def opcode = 0xD1 /+ 2
    override def hasImplicitOperand = true
  }

  implicit object RCL_2 extends RCL._1[rm64] {
    def opcode = 0xD1 /+ 2
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}

trait RCLImpl extends RCLLow {
  implicit object RCL_3 extends RCL._2[rm8, imm8] {
    def opcode = 0xC0 /+ 2
  }

  implicit object RCL_4 extends RCL._2[rm16, imm8] {
    def opcode = 0xC1 /+ 2
  }

  implicit object RCL_5 extends RCL._2[rm32, imm8] {
    def opcode = 0xC1 /+ 2
  }

  implicit object RCL_6 extends RCL._2[rm64, imm8] {
    def opcode = 0xC1 /+ 2
    override def prefix = REX.W(true)
  }

  implicit object RCL_7 extends RCL._1[rm8] {
    def opcode = 0xD0 /+ 2
    override def hasImplicitOperand = true
  }
}
