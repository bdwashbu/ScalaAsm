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

abstract class OneOperandFormat2[OpEn, -X <: Operand] extends OperandFormat {
  def getAddressingForm(x: X, opcode: OpcodeFormat): AddressingFormSpecifierTemp
}

abstract class TwoOperandsFormat[-X <: Operand, -Y <: Operand] extends OperandFormat {
  def getPrefixes(x: X, y: Y): Option[Array[Byte]]
  def getAddressingForm(x: X, y: Y, opcode: OpcodeFormat): AddressingFormSpecifierTemp
}

object MI extends TwoOperandsFormat[ModRM.rm, Immediate] {

  def getAddressingForm(op1: ModRM.rm, op2: Immediate, opcode: OpcodeFormat): AddressingFormSpecifierTemp = {

    op1 match {
      case reg: GPR =>
        new AddressingFormSpecifierTemp {
          val addressingForm = TwoOperandInstructionFormatter.encode(reg, op2, opcode.opcodeExtension)
          val (displacment, immediate) = (None, Some(op2))
        }
    }
  }

  def getPrefixes(op1: ModRM.rm, op2: Immediate): Option[Array[Byte]] = {
    op1 match {
      case reg: UniformByteRegister =>
        Some(REX.W(false).get)
      case reg: Register64 =>
        Some(REX.W(true).get)
      case _ => None
    }
  }
}

object RM extends TwoOperandsFormat[ModRM.reg, ModRM.rm] {

  def getAddressingForm(op1: ModRM.reg, op2: ModRM.rm, opcode: OpcodeFormat): AddressingFormSpecifierTemp = {

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

  def getPrefixes(op1: ModRM.reg, op2: ModRM.rm): Option[Array[Byte]] = {
    op1 match {
      case reg: UniformByteRegister =>
        Some(REX.W(false).get)
      case reg: Register64 =>
        Some(REX.W(true).get)
      case _ => None
    }
  }
}

object MR extends TwoOperandsFormat[ModRM.rm, ModRM.reg] {

  def getAddressingForm(op1:  ModRM.rm, op2: ModRM.reg, opcode: OpcodeFormat): AddressingFormSpecifierTemp = {
    RM.getAddressingForm(op2, op1, opcode)
  }

  def getPrefixes(op1:  ModRM.rm, op2: ModRM.reg): Option[Array[Byte]] = RM.getPrefixes(op2, op1)
}

object OI extends TwoOperandsFormat[ModRM.plusRd, Immediate] {

  def getAddressingForm(op1: ModRM.plusRd, op2: Immediate, opcode: OpcodeFormat): AddressingFormSpecifierTemp = {
    new AddressingFormSpecifierTemp {
      val addressingForm = NoModRM()
      val (displacment, immediate) = (None, Some(op2))
    }
  }

  def getPrefixes(op1: ModRM.plusRd, op2: Immediate): Option[Array[Byte]] = {
    op1 match {
      case reg: Register64 =>
        Some(REX.W(true).get)
      case _ => None
    }
  }
}

object M1 extends TwoOperandsFormat[ModRM.rm, One] with Formats {
  def getAddressingForm(op1: ModRM.rm, op2: One, opcode: OpcodeFormat): AddressingFormSpecifierTemp = MFormat.getAddressingForm(op1, opcode)
  def getPrefixes(op1: ModRM.rm, op2: One): Option[Array[Byte]] = None
}



