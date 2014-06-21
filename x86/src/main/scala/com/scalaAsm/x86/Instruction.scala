package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.ModRM
import com.scalaAsm.x86.Operands.Memory.InstructionFormat
import com.scalaAsm.x86.Operands.Memory.NoModRM
import com.scalaAsm.x86.Operands.Memory.NoSIBWithDisplacement
import com.scalaAsm.x86.Operands.Memory.NoDisplacement
import com.scalaAsm.x86.Operands.Memory.ModRMOpcode
import com.scalaAsm.x86.Operands.Memory.OnlyDisplacement
import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.x86.Operands.Memory.RegisterIndirect
import com.scalaAsm.x86.Operands.Memory.BaseIndex

trait Instruction

trait InstructionField {
  def getBytes: Array[Byte]
  def size: Int
}

trait x86Instruction extends Instruction {
  import scala.language.implicitConversions
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

trait OperandEncoding {
  type Immediate = Constant
}

trait Formats extends OperandEncoding {

  implicit object MFormat extends OneOperandFormat[M, ModRM.rm] {

    def getAddressingForm(op1: ModRM.rm, opcode: OpcodeFormat): InstructionFormat = {

      op1 match {
        case rel: Relative =>
          InstructionFormat (
            addressingForm = rel.encode(opcode.opcodeExtension),
            immediate = None
          )
        case mem: AbsoluteAddress =>
          InstructionFormat (
            addressingForm = mem.encode(opcode.opcodeExtension),
            immediate = None
          )
        case mem: RegisterIndirect =>
          InstructionFormat (
            addressingForm = mem.encode(opcode.opcodeExtension),
            immediate = None
          )
        case mem: BaseIndex =>
          InstructionFormat (
            addressingForm = mem.encode(opcode.opcodeExtension),
            immediate = None
          )
        case reg: GPR =>
          InstructionFormat (
            addressingForm = reg.encode(opcode.opcodeExtension),
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
      InstructionFormat (
        addressingForm = NoModRM(),
        immediate = None
      )
    }
  }

  implicit object IFormat extends OneOperandFormat[I, Immediate] {

    def getAddressingForm(op1: Immediate, opcode: OpcodeFormat) = {
      InstructionFormat (
        addressingForm = NoModRM(),
        immediate = Some(op1)
      )
    }

  }

  implicit object OffsetFormat extends OneOperandFormat[Offset, BaseIndex] {

    def getAddressingForm(op1: BaseIndex, opcode: OpcodeFormat) = {
      InstructionFormat (
        addressingForm = op1.encode(opcode.opcodeExtension),//NoSIBWithDisplacement(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, new EBP), op1.offset),
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

trait Formats2 extends OperandEncoding {

  implicit object MIFormat extends TwoOperandsFormat[MI, ModRM.rm, Immediate] {

    def getAddressingForm(op1: ModRM.rm, op2: Immediate, opcode: OpcodeFormat): InstructionFormat = {

      op1 match {
        case reg: GPR =>
          InstructionFormat (
            addressingForm = reg.encode(opcode.opcodeExtension),
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

    def getAddressingForm(op1: ModRM.reg, op2: ModRM.rm, opcode: OpcodeFormat): InstructionFormat = {

      op2 match {
        case mem: AbsoluteAddress =>
          InstructionFormat (
            addressingForm = mem.encode(opcode.opcodeExtension),
            immediate = None
          )
        case mem: RegisterIndirect =>
          InstructionFormat (
            addressingForm = mem.encode(op1, opcode.opcodeExtension),
            immediate = None
          )
        case mem: BaseIndex =>
          InstructionFormat (
            addressingForm = mem.encode(op1, opcode.opcodeExtension),
            immediate = None
          )
        case reg: GPR =>
          InstructionFormat (
            addressingForm = op1.encode(reg, opcode.opcodeExtension),
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

    def getAddressingForm(op1: ModRM.rm, op2: ModRM.reg, opcode: OpcodeFormat): InstructionFormat = {
      RMFormat.getAddressingForm(op2, op1, opcode)
    }

    def getPrefixes(op1: ModRM.rm, op2: ModRM.reg): Option[Array[Byte]] = RMFormat.getPrefixes(op2, op1)
  }

  implicit object OIFormat extends TwoOperandsFormat[OI, ModRM.plusRd, Immediate] {

    def getAddressingForm(op1: ModRM.plusRd, op2: Immediate, opcode: OpcodeFormat): InstructionFormat = {
      InstructionFormat (
        addressingForm = NoModRM(),
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
    def getAddressingForm(op1: ModRM.rm, op2: One, opcode: OpcodeFormat): InstructionFormat = MFormat.getAddressingForm(op1, opcode)
    def getPrefixes(op1: ModRM.rm, op2: One): Option[Array[Byte]] = None
  }
}

case class OneOperandMachineCodeBuilder[O1 <: Operand, X](operand: O1, opcode: OpcodeFormat, mnemonic: String, format: OneOperandFormat[X, O1]) extends MachineCodeBuilder{
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

case class TwoOperandMachineCodeBuilder[O1 <: Operand, O2 <: Operand, X](operand: O1, operand2: O2, opcode: OpcodeFormat, mnemonic: String, format: TwoOperandsFormat[X, O1, O2]) extends MachineCodeBuilder {
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
