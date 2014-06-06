package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.ModRM
import com.scalaAsm.x86.Operands.Memory.InstructionConstants
import com.scalaAsm.x86.Operands.Memory.NoModRM
import com.scalaAsm.x86.Operands.Memory.NoSIB
import com.scalaAsm.x86.Operands.Memory.NoDisplacement
import com.scalaAsm.x86.Operands.Memory.ModRMOpcode

trait Instruction

trait InstructionField {
  def getBytes: Array[Byte]
  def size: Int
}

trait x86Instruction extends Instruction {
  val mnemonic: String
  implicit def toByte(x: Int) = x.toByte
  implicit def toOneOpcode(x: Int): OneOpcode = OneOpcode(x.toByte)
  implicit def toTwoOpcodes(x: (Int, Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte)
}

trait ZeroOperandInstruction extends x86Instruction {
  def opcode: OpcodeFormat

  def apply: MachineCodeBuilder =
    new MachineCodeBuilder {
      def get = new MachineCode {
        val size = getSize
        val code = getBytes
        val line = mnemonic
      }
    }

  def getSize: Int = {
    opcode.size
  }

  def getBytes: Array[Byte] = {
    opcode.get(null)
  }
}

trait M
trait O
trait I
trait Offset

trait Formats {

  implicit object MFormat extends OneOperandFormat[M, ModRM.rm] {

    def getAddressingForm(op1: ModRM.rm, opcode: OpcodeFormat): InstructionConstants = {

      op1 match {
        case rel: Relative =>
          InstructionConstants (
            addressingForm = NoModRM(),
            displacement = Some(rel.offset),
            immediate = None
          )
        case mem: ImmediateMemory =>
          InstructionConstants (
            addressingForm = mem.encode(opcode.opcodeExtension),
            displacement = None,
            immediate = Some(mem.immediate)
          )
        case mem: RegisterIndirect =>
          InstructionConstants (
            addressingForm = mem.encode(opcode.opcodeExtension),
            displacement = None,
            immediate = None
          )
        case mem: BaseIndex =>
          InstructionConstants (
            addressingForm = mem.encode(opcode.opcodeExtension),
            displacement = Some(mem.offset),
            immediate = None
          )
        case reg: GPR =>
          InstructionConstants (
            addressingForm = reg.encode(opcode.opcodeExtension),
            displacement = None,
            immediate = None
          )
      }
    }
  }

  implicit object DSFormat extends OneOperandFormat[DS, DS] {
    def getAddressingForm(op1: DS, opcode: OpcodeFormat) = NoAddressingForm
  }

  implicit object CSFormat extends OneOperandFormat[CS, CS] {
    def getAddressingForm(op1: CS, opcode: OpcodeFormat) = NoAddressingForm
  }

  implicit object OFormat extends OneOperandFormat[O, ModRM.plusRd] {
    def getAddressingForm(op1: ModRM.plusRd, opcode: OpcodeFormat) = {
      InstructionConstants (
        addressingForm = NoModRM(),
        displacement = None,
        immediate = None
      )
    }
  }

  implicit object IFormat extends OneOperandFormat[I, Immediate] {

    def getAddressingForm(op1: Immediate, opcode: OpcodeFormat) = {
      InstructionConstants (
        addressingForm = NoModRM(),
        displacement = None,
        immediate = Some(op1)
      )
    }

  }

  implicit object OffsetFormat extends OneOperandFormat[Offset, BaseIndex] {

    def getAddressingForm(op1: BaseIndex, opcode: OpcodeFormat) = {
      InstructionConstants (
        addressingForm = NoSIB(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, new EBP)),
        displacement = Some(op1.offset),
        immediate = None
      )
    }
  }
}

trait MR
trait OI
trait RM
trait M1
trait MI

trait Formats2 {
  implicit object MIFormat extends TwoOperandsFormat[MI, ModRM.rm, Immediate] {

    def getAddressingForm(op1: ModRM.rm, op2: Immediate, opcode: OpcodeFormat): InstructionConstants = {

      op1 match {
        case reg: GPR =>
          InstructionConstants (
            addressingForm = reg.encode(opcode.opcodeExtension),
            displacement = None,
            immediate = Some(op2)
          )
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

  implicit object RMFormat extends TwoOperandsFormat[RM, ModRM.reg, ModRM.rm] {

    def getAddressingForm(op1: ModRM.reg, op2: ModRM.rm, opcode: OpcodeFormat): InstructionConstants = {

      op2 match {
        case mem: ImmediateMemory =>
          InstructionConstants (
            addressingForm = mem.encode(opcode.opcodeExtension),
            displacement = None,
            immediate = Some(mem.immediate)
          )
        case mem: RegisterIndirect =>
          InstructionConstants (
            addressingForm = mem.encode(op1, opcode.opcodeExtension),
            displacement = None,
            immediate = None
          )
        case mem: BaseIndex =>
          InstructionConstants (
            addressingForm = op1.encode(mem, opcode.opcodeExtension),
            displacement = Some(mem.offset),
            immediate = None
          )
        case reg: GPR =>
          InstructionConstants (
            addressingForm = op1.encode(reg, opcode.opcodeExtension),
            displacement = None,
            immediate = None
          )
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

  implicit object MRFormat extends TwoOperandsFormat[MR, ModRM.rm, ModRM.reg] {

    def getAddressingForm(op1: ModRM.rm, op2: ModRM.reg, opcode: OpcodeFormat): InstructionConstants = {
      RMFormat.getAddressingForm(op2, op1, opcode)
    }

    def getPrefixes(op1: ModRM.rm, op2: ModRM.reg): Option[Array[Byte]] = RMFormat.getPrefixes(op2, op1)
  }

  implicit object OIFormat extends TwoOperandsFormat[OI, ModRM.plusRd, Immediate] {

    def getAddressingForm(op1: ModRM.plusRd, op2: Immediate, opcode: OpcodeFormat): InstructionConstants = {
      InstructionConstants (
        addressingForm = NoModRM(),
        displacement = None,
        immediate = Some(op2)
      )
    }

    def getPrefixes(op1: ModRM.plusRd, op2: Immediate): Option[Array[Byte]] = {
      op1 match {
        case reg: Register64 =>
          Some(REX.W(true).get)
        case _ => None
      }
    }
  }

  implicit object M1Format extends TwoOperandsFormat[M1, ModRM.rm, One] with Formats {
    def getAddressingForm(op1: ModRM.rm, op2: One, opcode: OpcodeFormat): InstructionConstants = MFormat.getAddressingForm(op1, opcode)
    def getPrefixes(op1: ModRM.rm, op2: One): Option[Array[Byte]] = None
  }
}

case class OneOperandMachineCodeBuilder[O1 <: Operand](operand: O1, opcode: OpcodeFormat, mnemonic: String, format: OneOperandFormat[_, O1]) extends MachineCodeBuilder{
  def get() =
    new MachineCode {
        val size = getSize
        val code = getBytes
        val line = mnemonic
      }

  def getSize: Int = {
    opcode.size + format.getAddressingForm(operand, opcode).size
  }

  def getBytes: Array[Byte] = {
    opcode.get(operand) ++ format.getAddressingForm(operand, opcode).getBytes
  }
}

case class TwoOperandMachineCodeBuilder[O1 <: Operand, O2 <: Operand](operand: O1, operand2: O2, opcode: OpcodeFormat, mnemonic: String, format: TwoOperandsFormat[_, O1, O2]) extends MachineCodeBuilder {
  def get() =
    new MachineCode {
        val size = getSize
        val code = getBytes
        val line = mnemonic
      }

  def getSize: Int = {
    val prefixes = format.getPrefixes(operand, operand2) getOrElse Array()
    prefixes.size + opcode.size + format.getAddressingForm(operand, operand2, opcode).size
  }

  def getBytes: Array[Byte] = {
    val prefixes = format.getPrefixes(operand, operand2) getOrElse Array()
    prefixes ++ opcode.get(operand) ++ format.getAddressingForm(operand, operand2, opcode).getBytes
  }
}

abstract class OneOperandInstruction[OpEn, -O1 <: Operand](implicit format: OneOperandFormat[OpEn, O1]) extends x86Instruction with Formats {
  val opcode: OpcodeFormat

  def get[X <: O1](x: X) = OneOperandMachineCodeBuilder(x, opcode, mnemonic, format)
}

abstract class TwoOperandInstruction[OpEn, -O1 <: Operand, -O2 <: Operand](implicit format: TwoOperandsFormat[OpEn, O1, O2]) extends x86Instruction with Formats2{
  val opcode: OpcodeFormat

  def get[X <: O1, Y <: O2](x: X, y:Y) = TwoOperandMachineCodeBuilder(x, y, opcode, mnemonic, format)
}
