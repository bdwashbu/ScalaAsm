package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

trait OperandFormat

object NoOperand extends OperandFormat

object NoAddressingForm extends AddressingFormSpecifierTemp {
  val (addressingForm, displacment, immediate) = (NoModRM(), None, None)
}

abstract class OneOperandFormat[-X <: Operand] extends OperandFormat {
  def getAddressingForm(x: X, opcode: OpcodeFormat): AddressingFormSpecifierTemp
}

abstract class TwoOperandsFormat[-X <: Operand, -Y <: Operand] extends OperandFormat {
  def getPrefixes(x: X, y: Y): Option[Array[Byte]]
  def getAddressingForm(x: X, y: Y, opcode: OpcodeFormat): AddressingFormSpecifierTemp
}

case class MI[M <: ModRM.rm, I <: Immediate]() extends TwoOperandsFormat[M, I] {

  def getAddressingForm(op1: M, op2: I, opcode: OpcodeFormat): AddressingFormSpecifierTemp = {

    op1 match {
      case reg: GPR =>
        new AddressingFormSpecifierTemp {
          val addressingForm = TwoOperandInstructionFormatter.encode(reg, op2, opcode.opcodeExtension)
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

  def getAddressingForm(op1: R, op2: M, opcode: OpcodeFormat): AddressingFormSpecifierTemp = {

    op2 match {
      case mem: Memory =>
        new AddressingFormSpecifierTemp {
          val addressingForm = TwoOperandInstructionFormatter.encode(op1, mem, opcode.opcodeExtension)
          val (displacment, immediate) = (mem.offset, mem.immediate)
        }
      case reg: GPR =>
        new AddressingFormSpecifierTemp {
          val addressingForm = TwoOperandInstructionFormatter.encode(op1, reg, opcode.opcodeExtension)
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

  def getAddressingForm(op1: M, op2: R, opcode: OpcodeFormat): AddressingFormSpecifierTemp = {
    RM().getAddressingForm(op2, op1, opcode)
  }

  def getPrefixes(op1: M, op2: R): Option[Array[Byte]] = RM().getPrefixes(op2, op1)
}

case class OI[O <: ModRM.plusRd, I <: Immediate]() extends TwoOperandsFormat[O, I] {

  def getAddressingForm(op1: O, op2: I, opcode: OpcodeFormat): AddressingFormSpecifierTemp = {
    new AddressingFormSpecifierTemp {
      val addressingForm = NoModRM()
      val (displacment, immediate) = (None, Some(op2))
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
  def getAddressingForm(op1: M, op2: One, opcode: OpcodeFormat): AddressingFormSpecifierTemp = M().getAddressingForm(op1, opcode)
  def getPrefixes(op1: M, op2: One): Option[Array[Byte]] = None
}

case class O[O <: ModRM.plusRd]() extends OneOperandFormat[O] {
  def getAddressingForm(op1: O, opcode: OpcodeFormat) = {
    new AddressingFormSpecifierTemp {
      val addressingForm = NoModRM()
      val (displacment, immediate) = (None, None)
    }
  }
}

case class I[I <: Immediate]() extends OneOperandFormat[I] {

  def getAddressingForm(op1: I, opcode: OpcodeFormat) = {
    new AddressingFormSpecifierTemp {
      val addressingForm = NoModRM()
      val (displacment, immediate) = (None, Some(op1))
    }
  }

}

case class Offset[O <: Memory]() extends OneOperandFormat[O] {

  def getAddressingForm(op1: O, opcode: OpcodeFormat) = {
    new AddressingFormSpecifierTemp {
      val addressingForm = NoSIB(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, new EBP))
      val (displacment, immediate) = (op1.offset, None)
    }
  }
}

case class M[M <: ModRM.rm]() extends OneOperandFormat[M] {

  def getAddressingForm(op1: M, opcode: OpcodeFormat) = {

    op1 match {
      case rel: Relative =>
        new AddressingFormSpecifierTemp {
          val addressingForm = NoModRM()
          val (displacment, immediate) = (rel.offset, None)
        }
      case mem: Memory =>
        new AddressingFormSpecifierTemp {
          val addressingForm = OneOperandInstructionFormatter.encode(mem, opcode.opcodeExtension)
          val (displacment, immediate) = (mem.offset, mem.immediate)
        }
      case reg: GPR =>
        new AddressingFormSpecifierTemp {
          val addressingForm = OneOperandInstructionFormatter.encode(reg, opcode.opcodeExtension)
          val (displacment, immediate) = (None, None)
        }
    }
  }
}

