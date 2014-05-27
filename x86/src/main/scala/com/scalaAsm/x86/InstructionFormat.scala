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

  def encode(op1: Memory, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    (op1.base, op1.offset, op1.immediate) match {
      case (Some(base: Register64), _, _) =>
        WithSIB(ModRMOpcode(NoDisplacement, opcodeExtend.get, base), SIB(SIB.One, new ESP, base))
      case (Some(base), Some(_: Displacement8), None) =>
        NoSIB(ModRMOpcode(DisplacementByte, opcodeExtend.get, base))
      case (Some(base), None, None) =>
        NoSIB(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
      case (None, None, Some(_: Immediate)) =>
        NoSIB(ModRMOpcode(NoDisplacement, opcodeExtend.get, new EBP))
      case _ => NoModRM()
    }
  }

  def encode(op1: GPR, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    NoSIB(ModRMOpcode(TwoRegisters, opcodeExtend.get, op1))
  }
}

object TwoOperandInstructionFormatter {

  def encode(op1: GPR, op2: Immediate, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    NoSIB(ModRMOpcode(TwoRegisters, opcodeExtend.get, op1))
  }

  def encode(op1: GPR, op2: Memory, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    (op2.base, op2.offset, op2.immediate) match {
      case (Some(base), Some(off: Displacement8), None) if base.ID == 4 =>
        WithSIB(ModRMReg(DisplacementByte, op1, base), SIB(SIB.One, new ESP, base))
      case (Some(base), Some(off: Displacement32), None) =>
        NoSIB(ModRMReg(DisplacementDword, reg = op1, rm = base))
      case (Some(base), Some(_: Displacement), None) =>
        NoSIB(ModRMReg(DisplacementByte, reg = op1, rm = base))
      case (Some(base), None, None) =>
        NoSIB(ModRMReg(NoDisplacement, reg = op1, rm = base))
    }
  }

  def encode(op1: GPR, op2: GPR, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    NoSIB(ModRMReg(TwoRegisters, op1, op2))
  }
}