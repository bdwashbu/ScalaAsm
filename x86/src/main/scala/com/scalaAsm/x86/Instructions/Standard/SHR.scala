package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object SHR extends InstructionDefinition[OneOpcode]("SHR") with SHRImpl

trait SHRLow {
  implicit object SHR_209_rm16 extends SHR._1_new[rm16] {
    def opcode = 0xD1 /+ 5
    override def hasImplicateOperand = true
  }

  implicit object SHR_209_rm32 extends SHR._1_new[rm32] {
    def opcode = 0xD1 /+ 5
    override def hasImplicateOperand = true
  }

  implicit object SHR_209_rm64 extends SHR._1_new[rm64] {
    def opcode = 0xD1 /+ 5
    override def prefix = REX.W(true)
    override def hasImplicateOperand = true
  }
}

trait SHRImpl extends SHRLow {
  implicit object SHR_192_rm8_imm8 extends SHR._2_new[rm8, imm8] {
    def opcode = 0xC0 /+ 5
  }

  implicit object SHR_193_rm16_imm8 extends SHR._2_new[rm16, imm8] {
    def opcode = 0xC1 /+ 5
  }

  implicit object SHR_193_rm32_imm8 extends SHR._2_new[rm32, imm8] {
    def opcode = 0xC1 /+ 5
  }

  implicit object SHR_193_rm64_imm8 extends SHR._2_new[rm64, imm8] {
    def opcode = 0xC1 /+ 5
    override def prefix = REX.W(true)
  }

  implicit object SHR_208_rm8 extends SHR._1_new[rm8] {
    def opcode = 0xD0 /+ 5
    override def hasImplicateOperand = true
  }

  implicit object SHR_210_rm8_CL extends SHR._2_new[rm8, CL] {
    def opcode = 0xD2 /+ 5
  }

  implicit object SHR_211_rm16_CL extends SHR._2_new[rm16, CL] {
    def opcode = 0xD3 /+ 5
  }

  implicit object SHR_211_rm32_CL extends SHR._2_new[rm32, CL] {
    def opcode = 0xD3 /+ 5
  }

  implicit object SHR_211_rm64_CL extends SHR._2_new[rm64, CL] {
    def opcode = 0xD3 /+ 5
    override def prefix = REX.W(true)
  }
}
