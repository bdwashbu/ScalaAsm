package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object MOV extends InstructionDefinition[OneOpcode]("MOV") with MOVImpl

trait MOVLow {
 
  
   implicit object MOV_137_rm16_r16 extends MOV._2[rm16, r16] {
    def opcode = 0x89 /r
    }

  implicit object MOV_137_rm32_r32 extends MOV._2[rm32, r32] {
    def opcode = 0x89 /r
    override def explicitFormat(op1: rm32, op2: r32) = {
     if (op1.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1.asInstanceOf[reg], op2)), immediate = None))
     } else None
   }  }

  implicit object MOV_137_rm64_r64 extends MOV._2[rm64, r64] {
    def opcode = 0x89 /r
    override def prefix = REX.W(true)
    }
  
   implicit object MOV_199_rm16_imm16 extends MOV._2[rm16, imm16] {
    def opcode = 0xC7 /+ 0
    }

  implicit object MOV_199_rm32_imm32 extends MOV._2[rm32, imm32] {
    def opcode = 0xC7 /+ 0
    }

  implicit object MOV_199_rm64_imm32 extends MOV._2[rm64, imm32] {
    def opcode = 0xC7 /+ 0
    override def prefix = REX.W(true)
    }
}

trait MOVImpl extends MOVLow {
  implicit object MOV_136_rm8_r8 extends MOV._2[rm8, r8] {
    def opcode = 0x88 /r
    }

  implicit object MOV_139_r16_rm16 extends MOV._2[r16, rm16] {
    def opcode = 0x8B /r
    }

  implicit object MOV_139_r32_rm32 extends MOV._2[r32, rm32] {
    def opcode = 0x8B /r
    override def explicitFormat(op1: r32, op2: rm32) = {
     if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
     } else None
   }  }

  implicit object MOV_139_r64_rm64 extends MOV._2[r64, rm64] {
    def opcode = 0x8B /r
    override def prefix = REX.W(true)
    }

  implicit object MOV_138_r8_rm8 extends MOV._2[r8, rm8] {
    def opcode = 0x8A /r
    }

 

  implicit object MOV_176_r8_imm8 extends MOV._2[r8, imm8] {
    def opcode = 0xB0 + rb
    }

  implicit object MOV_184_r16_imm16 extends MOV._2[r16, imm16] {
    def opcode = 0xB8 + rw
    }

  implicit object MOV_184_r32_imm32 extends MOV._2[r32, imm32] {
    def opcode = 0xB8 + rd
    }

  implicit object MOV_184_r64_imm64 extends MOV._2[r64, imm64] {
    def opcode = 0xB8 + ro
    override def prefix = REX.W(true)
    }

  implicit object MOV_198_rm8_imm8 extends MOV._2[rm8, imm8] {
    def opcode = 0xC6 /+ 0
    }

 
}
