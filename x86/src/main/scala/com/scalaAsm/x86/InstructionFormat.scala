package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

trait InstructionFormat

case class Format1(opcode: OpcodeFormat) extends InstructionFormat {
  val size = 1
  val getBytes = opcode.get(null)
}

case class Format2(opcode: OpcodeFormat, imm: Immediate8) extends InstructionFormat {
  val size = 2
  val getBytes = opcode.get(null) ++ imm.getBytes
}

case class Format3(opcode: OpcodeFormat, disp: Displacement) extends InstructionFormat {
  val size = 1 + disp.size
  val getBytes = opcode.get(null) ++ disp.getBytes
}

case class Format9(opcode: OpcodeFormat) extends InstructionFormat {
  val size = 2
  val getBytes = opcode.get(null)
}

object OneOperandInstructionFormatter {

  def encode(op1: Memory, opcodeExtend: Option[Byte]): (Option[ModRM], Option[SIB]) = {
    (op1.base, op1.offset, op1.immediate) match {
      case (Some(base: Register64), _, _) =>
        val modRM = Some(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
        val sib = Some(SIB(SIB.One, new ESP, base))
        (modRM, sib)
      case (Some(base), offset @ Some(_: Displacement8), None) =>
        val modRM = Some(ModRMOpcode(DisplacementByte, opcodeExtend.get, base))
        (modRM, None)
      case (Some(base), None, None) =>
        val modRM = Some(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
        (modRM, None)
      case (None, None, imm @ Some(_: Immediate)) =>
        val modRM = Some(ModRMOpcode(NoDisplacement, opcodeExtend.get, new EBP))
        (modRM, None)
      case _ => (None, None)
    }
  }

  def encode(op1: GPR, opcodeExtend: Option[Byte]): (Option[ModRM], Option[SIB]) = {
    val modRM = Some(ModRMOpcode(TwoRegisters, opcodeExtend.get, op1))
    (modRM, None)
  }
}

object TwoOperandInstructionFormatter {

  def encode(op1: GPR, op2: Immediate, opcodeExtend: Option[Byte]): (Option[ModRM], Option[SIB]) = {
    val modRM: Option[ModRM] = Some(ModRMOpcode(TwoRegisters, opcodeExtend.get, op1))
    (modRM, None)
  }

  def encode(op1: GPR, op2: Memory, opcodeExtend: Option[Byte]): (Option[ModRM], Option[SIB]) = {
    (op2.base, op2.offset, op2.immediate) match {
      case (Some(base), offset @ Some(off: Displacement8), None) if base.ID == 4 =>
        val modRM = Some(ModRMReg(DisplacementByte, op1, base))
        val sib = Some(SIB(SIB.One, new ESP, base))
        (modRM, sib)
      case (Some(base), offset @ Some(off: Displacement32), None) =>
        val modRM = Some(ModRMReg(DisplacementDword, reg = op1, rm = base))
        (modRM, None)
      case (Some(base), offset @ Some(_: Displacement), None) =>
        val modRM = Some(ModRMReg(DisplacementByte, reg = op1, rm = base))
        (modRM, None)
      case (Some(base), None, None) =>
        val modRM = Some(ModRMReg(NoDisplacement, reg = op1, rm = base))
        (modRM, None)
    }
  }

  def encode(op1: GPR, op2: GPR, opcodeExtend: Option[Byte]): (Option[ModRM], Option[SIB]) = {
    val modRM: Option[ModRM] = Some(ModRMReg(TwoRegisters, op1, op2))
    (modRM, None)
  }
}