package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

trait OperandFormat

object NoOperand extends OperandFormat

object NoAddressingForm extends AddressingFormSpecifier {
  val (modRM, sib, displacment, immediate) = (None, None, None, None)
}

abstract class OneOperandFormat[-X <: Operand] extends OperandFormat {
  def getAddressingForm(x: X, opcode: OpcodeFormat): AddressingFormSpecifier
}

abstract class TwoOperandsFormat[-X <: Operand, -Y <: Operand] extends OperandFormat {
  def getPrefixes(x: X, y: Y): Option[Array[Byte]]
  def getAddressingForm(x: X, y: Y, opcode: OpcodeFormat): AddressingFormSpecifier
}

case class MI[M <: ModRM.rm, I <: Immediate]() extends TwoOperandsFormat[M, I] {

  def getAddressingForm(op1: M, op2: I, opcode: OpcodeFormat): AddressingFormSpecifier = {

    op1 match {
      case reg: GPR =>
        new AddressingFormSpecifier {
          val (modRM, sib) = TwoOperandInstructionFormatter.encode(reg, op2, opcode.opcodeExtension)
          val (displacment, immediate) = (None, Some(op2))
        }
    }
  }

  def getPrefixes(op1: M, op2: I): Option[Array[Byte]] = {
    op1 match {
      case reg: UniformByteRegister =>
        Some(REX.W(false).get)
      case reg: Register64 =>
        Some(REX.W(true).get)
      case _ => None
    }
  }
}

case class RM[R <: ModRM.reg, M <: ModRM.rm]() extends TwoOperandsFormat[R, M] {

  def getAddressingForm(op1: R, op2: M, opcode: OpcodeFormat): AddressingFormSpecifier = {

    op2 match {
      case mem: Memory =>
        new AddressingFormSpecifier {
          val (modRM, sib) = TwoOperandInstructionFormatter.encode(op1, mem, opcode.opcodeExtension)
          val (displacment, immediate) = (mem.offset, mem.immediate)
        }
      case reg: GPR =>
        new AddressingFormSpecifier {
          val (modRM, sib) = TwoOperandInstructionFormatter.encode(op1, reg, opcode.opcodeExtension)
          val (displacment, immediate) = (None, None)
        }
    }
  }

  def getPrefixes(op1: R, op2: M): Option[Array[Byte]] = {
    op1 match {
      case reg: UniformByteRegister =>
        Some(REX.W(false).get)
      case reg: Register64 =>
        Some(REX.W(true).get)
      case _ => None
    }
  }
}

case class MR[M <: ModRM.rm, R <: ModRM.reg]() extends TwoOperandsFormat[M, R] {

  def getAddressingForm(op1: M, op2: R, opcode: OpcodeFormat): AddressingFormSpecifier = {
    RM().getAddressingForm(op2, op1, opcode)
  }

  def getPrefixes(op1: M, op2: R): Option[Array[Byte]] = RM().getPrefixes(op2, op1)
}

case class OI[O <: ModRM.plusRd, I <: Immediate]() extends TwoOperandsFormat[O, I] {

  def getAddressingForm(op1: O, op2: I, opcode: OpcodeFormat): AddressingFormSpecifier = {
    new AddressingFormSpecifier {
      val modRM = None
      val (sib, displacment, immediate) = (None, None, Some(op2))
    }
  }

  def getPrefixes(op1: O, op2: I): Option[Array[Byte]] = {
    op1 match {
      case reg: Register64 =>
        Some(REX.W(true).get)
      case _ => None
    }
  }
}

case class M1[M <: ModRM.rm]() extends TwoOperandsFormat[M, One] {
  def getAddressingForm(op1: M, op2: One, opcode: OpcodeFormat): AddressingFormSpecifier = M().getAddressingForm(op1, opcode)
  def getPrefixes(op1: M, op2: One): Option[Array[Byte]] = None
}

case class O[O <: ModRM.plusRd]() extends OneOperandFormat[O] {
  def getAddressingForm(op1: O, opcode: OpcodeFormat) = {
    new AddressingFormSpecifier {
      val modRM = None
      val (sib, displacment, immediate) = (None, None, None)
    }
  }
}

case class I[I <: Immediate]() extends OneOperandFormat[I] {

  def getAddressingForm(op1: I, opcode: OpcodeFormat) = {
    new AddressingFormSpecifier {
      val modRM = None
      val (sib, displacment, immediate) = (None, None, Some(op1))
    }
  }

}

case class Offset[O <: Memory]() extends OneOperandFormat[O] {

  def getAddressingForm(op1: O, opcode: OpcodeFormat) = {
    new AddressingFormSpecifier {
      val modRM = Some(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, new EBP))
      val (sib, displacment, immediate) = (None, op1.offset, None)
    }
  }
}

case class M[M <: ModRM.rm]() extends OneOperandFormat[M] {

  def getAddressingForm(op1: M, opcode: OpcodeFormat) = {

    op1 match {
      case rel: Relative =>
        new AddressingFormSpecifier {
          val (modRM, sib) = (None, None)
          val (displacment, immediate) = (rel.offset, None)
        }
      case mem: Memory =>
        new AddressingFormSpecifier {
          val (modRM, sib) = OneOperandInstructionFormatter.encode(mem, opcode.opcodeExtension)
          val (displacment, immediate) = (mem.offset, mem.immediate)
        }
      case reg: GPR =>
        new AddressingFormSpecifier {
          val (modRM, sib) = OneOperandInstructionFormatter.encode(reg, opcode.opcodeExtension)
          val (displacment, immediate) = (None, None)
        }
    }
  }
}

