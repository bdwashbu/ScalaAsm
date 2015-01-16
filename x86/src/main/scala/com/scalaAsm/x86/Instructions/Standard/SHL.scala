package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object SHL extends InstructionDefinition[OneOpcode]("SHL") with SHLImpl

// Shift

trait SHLLow {
  implicit object SHL_209_rm16 extends SHL._1[rm16] {
    def opcode = 0xD1 /+ 4
    override def hasImplicateOperand = true
  }

  implicit object SHL_209_rm32 extends SHL._1[rm32] {
    def opcode = 0xD1 /+ 4
    override def hasImplicateOperand = true
  }

  implicit object SHL_209_rm64 extends SHL._1[rm64] {
    def opcode = 0xD1 /+ 4
    override def prefix = REX.W(true)
    override def hasImplicateOperand = true
  }
}

trait SHLImpl extends SHLLow {
  implicit object SHL_192_rm8_imm8 extends SHL._2[rm8, imm8] {
    def opcode = 0xC0 /+ 4
  }

  implicit object SHL_193_rm16_imm8 extends SHL._2[rm16, imm8] {
    def opcode = 0xC1 /+ 4
  }

  implicit object SHL_193_rm32_imm8 extends SHL._2[rm32, imm8] {
    def opcode = 0xC1 /+ 4
  }

  implicit object SHL_193_rm64_imm8 extends SHL._2[rm64, imm8] {
    def opcode = 0xC1 /+ 4
    override def prefix = REX.W(true)
  }

  implicit object SHL_208_rm8 extends SHL._1[rm8] {
    def opcode = 0xD0 /+ 4
    override def hasImplicateOperand = true
  }

  implicit object SHL_210_rm8_CL extends SHL._2[rm8, CL] {
    def opcode = 0xD2 /+ 4
  }

  implicit object SHL_211_rm16_CL extends SHL._2[rm16, CL] {
    def opcode = 0xD3 /+ 4
  }

  implicit object SHL_211_rm32_CL extends SHL._2[rm32, CL] {
    def opcode = 0xD3 /+ 4
  }

  implicit object SHL_211_rm64_CL extends SHL._2[rm64, CL] {
    def opcode = 0xD3 /+ 4
    override def prefix = REX.W(true)
  }
}
