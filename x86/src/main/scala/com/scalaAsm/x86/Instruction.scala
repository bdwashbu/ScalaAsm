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
import com.scalaAsm.x86.Operands.One

trait Instruction

trait InstructionField {
  def getBytes: Array[Byte]
  def size: Int
}

trait x86Instruction extends Instruction {
  import scala.language.implicitConversions
  val opcode: OpcodeFormat
  val mnemonic: String
  implicit def toByte(x: Int) = x.toByte
  implicit def toOneOpcode(x: Int): OneOpcode = OneOpcode(x.toByte)
  implicit def toTwoOpcodes(x: (Int, Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte)
}

trait NP

trait M
trait O
trait I
trait Offset

trait OperandEncoding {
  type Immediate = Constant
}

trait Formats extends OperandEncoding {

  implicit object MFormat extends OperandFormat[M, OneOperand[ModRM.rm]] {

    def getAddressingForm(operand: OneOperand[ModRM.rm], opcode: OpcodeFormat): InstructionFormat = {

      operand._1 match {
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
    
     def getPrefixes(operand: OneOperand[ModRM.rm]): Option[Array[Byte]] = None
  }

  implicit object DSFormat extends NoOperandFormat[DS, OneOperand[DS]]

  implicit object CSFormat extends NoOperandFormat[CS, OneOperand[CS]]

  implicit object OFormat extends OperandFormat[O, OneOperand[ModRM.plusRd]] {
    def getAddressingForm(operand: OneOperand[ModRM.plusRd], opcode: OpcodeFormat) = {
      InstructionFormat (
        addressingForm = NoModRM(),
        immediate = None
      )
    }
    
    def getPrefixes(operand: OneOperand[ModRM.plusRd]): Option[Array[Byte]] = None
  }

  implicit object IFormat extends OperandFormat[I, OneOperand[Immediate]] {

    def getAddressingForm(operand: OneOperand[Immediate], opcode: OpcodeFormat) = {
      InstructionFormat (
        addressingForm = NoModRM(),
        immediate = Some(operand._1)
      )
    }
    
    def getPrefixes(operand: OneOperand[Immediate]): Option[Array[Byte]] = None
  }

  implicit object OffsetFormat extends OperandFormat[Offset, OneOperand[BaseIndex]] {

    def getAddressingForm(operand: OneOperand[BaseIndex], opcode: OpcodeFormat) = {
      InstructionFormat (
        addressingForm = operand._1.encode(opcode.opcodeExtension),//NoSIBWithDisplacement(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, new EBP), op1.offset),
        immediate = None
      )
    }
    
    def getPrefixes(operand: OneOperand[BaseIndex]): Option[Array[Byte]] = None
  }
}

trait MR
trait OI
trait RM
trait M1
trait MI

trait Formats2 extends OperandEncoding {

  implicit object MIFormat extends OperandFormat[MI, TwoOperands[ModRM.rm, Immediate]] {

    def getAddressingForm(operands: TwoOperands[ModRM.rm, Immediate], opcode: OpcodeFormat): InstructionFormat = {

      operands._1 match {
        case reg: GPR =>
          InstructionFormat (
            addressingForm = reg.encode(opcode.opcodeExtension),
            immediate = Some(operands._2)
          )
      }
    }

    def getPrefixes(operands: TwoOperands[ModRM.rm, Immediate]): Option[Array[Byte]] = {
      operands._1 match {
        case reg: UniformByteRegister =>
          Some(REX.W(false).get)
        case reg: Register64 =>
          Some(REX.W(true).get)
        case _ => None
      }
    }
  }

  implicit object RMFormat extends OperandFormat[RM, TwoOperands[ModRM.reg, ModRM.rm]] {

    def getAddressingForm(operands: TwoOperands[ModRM.reg, ModRM.rm], opcode: OpcodeFormat): InstructionFormat = {

      InstructionFormat (
        addressingForm = operands._2 match {
        case mem: AbsoluteAddress =>
          mem.encode(opcode.opcodeExtension)
        case mem: RegisterIndirect =>
          mem.encode(operands._1, opcode.opcodeExtension)
        case mem: BaseIndex =>
          mem.encode(operands._1, opcode.opcodeExtension)
        case reg: GPR =>
          operands._1.encode(reg, opcode.opcodeExtension)
      },
        immediate = None
      )
      
      
    }

    def getPrefixes(operands: TwoOperands[ModRM.reg, ModRM.rm]): Option[Array[Byte]] = {
      operands._1 match {
        case reg: UniformByteRegister =>
          Some(REX.W(false).get)
        case reg: Register64 =>
          Some(REX.W(true).get)
        case _ => None
      }
    }
  }

  implicit object MRFormat extends OperandFormat[MR, TwoOperands[ModRM.rm, ModRM.reg]] {

    def getAddressingForm(operands: TwoOperands[ModRM.rm, ModRM.reg], opcode: OpcodeFormat): InstructionFormat = {
      RMFormat.getAddressingForm(TwoOperands(operands._2, operands._1), opcode)
    }

    def getPrefixes(operands: TwoOperands[ModRM.rm, ModRM.reg]): Option[Array[Byte]] = RMFormat.getPrefixes(TwoOperands(operands._2, operands._1))
  }

  implicit object OIFormat extends OperandFormat[OI, TwoOperands[ModRM.plusRd, Immediate]] {

    def getAddressingForm(operands: TwoOperands[ModRM.plusRd, Immediate], opcode: OpcodeFormat): InstructionFormat = {
      InstructionFormat (
        addressingForm = NoModRM(),
        immediate = Some(operands._2)
      )
    }

    def getPrefixes(operands: TwoOperands[ModRM.plusRd, Immediate]): Option[Array[Byte]] = {
      operands._1 match {
        case reg: Register64 =>
          Some(REX.W(true).get)
        case _ => None
      }
    }
  }

  implicit object M1Format extends OperandFormat[M1, TwoOperands[ModRM.rm, One]] with Formats {
    def getAddressingForm(op1: TwoOperands[ModRM.rm,  One], opcode: OpcodeFormat): InstructionFormat = MFormat.getAddressingForm(OneOperand(op1._1), opcode)
    def getPrefixes(op1: TwoOperands[ModRM.rm, One]): Option[Array[Byte]] = MFormat.getPrefixes(OneOperand(op1._1))
  }
}

case class MachineCodeBuilder[O1 <: Operands, X](operand: O1, opcode: OpcodeFormat, mnemonic: String, format: OperandFormat[X, O1]) {
  def get() =
    new MachineCode {
        val size = getSize
        val code = getBytes
        val line = mnemonic
      }

  private def getSize: Int = {
    val prefixes = format.getPrefixes(operand) getOrElse Array()
    prefixes.size + opcode.size + format.getAddressingForm(operand, opcode).size
  }

  private def getBytes: Array[Byte] = {
    val prefixes = format.getPrefixes(operand) getOrElse Array()
    prefixes ++: opcode.get(operand) ++: format.getAddressingForm(operand, opcode).getBytes
  }
}

abstract class ZeroOperandInstruction extends x86Instruction with Formats {
  def get = MachineCodeBuilder(OneOperand(null), opcode, mnemonic, new NoOperandFormat[NP, OneOperand[_]])
}

abstract class OneOperandInstruction[OpEn, -O1 <: Operand](implicit format: OperandFormat[OpEn, OneOperand[O1]]) extends x86Instruction with Formats {
  def get[X <: O1](x: X) = MachineCodeBuilder(OneOperand(x), opcode, mnemonic, format)
}

abstract class TwoOperandInstruction[OpEn, -O1 <: Operand, -O2 <: Operand](implicit format: OperandFormat[OpEn, TwoOperands[O1, O2]]) extends x86Instruction with Formats2{
  def get[X <: O1, Y <: O2](x: X, y:Y) = MachineCodeBuilder(TwoOperands(x, y), opcode, mnemonic, format)
}
