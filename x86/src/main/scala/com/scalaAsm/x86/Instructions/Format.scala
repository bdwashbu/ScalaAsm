package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._
import com.scalaAsm.x86.OneOpcode
import com.scalaAsm.x86.OpcodeFormat
import com.scalaAsm.x86.REX
import com.scalaAsm.x86.TwoOpcodes
import scala.language.implicitConversions
import com.scalaAsm.x86.Operands._

trait Low {
  implicit object New_RMFormat extends NewTwoOperandFormat[reg, reg + _8] {

    def getAddressingForm(op1: reg, op2: reg + _8, opcodeExtension: Byte) = {
      InstructionFormat(
        NoSIBWithDisplacement(ModRMReg(DisplacementByte, reg = op1, rm = op2.base), op2.displacement),
        immediate = None)
    }
  }

  implicit object New_OIFormat32 extends NewTwoOperandFormat[rm, imm32] {

    def getAddressingForm(op1: rm, op2: imm32, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = NoModRM(),
        immediate = Some(op2))
    }
  }
}

trait Formats extends Low {

  implicit object New_IFormat32 extends NewOneOperandFormat[imm32] {

    def getAddressingForm(operand: imm32, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = NoModRM(),
        immediate = Some(operand))
    }
  }

  implicit object New_MFormat64 extends NewOneOperandFormat[rel64] {

    def getAddressingForm(operand: rel64, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = OnlyDisplacement(operand),
        immediate = None)
    }
  }

  trait MFormatIGeneric[X <: GeneralPurpose[_32]#Indirect] extends NewOneOperandFormat[X] {

    def getAddressingForm(operand: X, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = OnlyModRM(ModRMOpcode(NoDisplacement, opcodeExtension, operand.base)),
        immediate = None)
    }
  }

  implicit object New_MFormatIGeneric1 extends MFormatIGeneric[EAX#Indirect]
  implicit object New_MFormatIGeneric2 extends MFormatIGeneric[ECX#Indirect]
  implicit object New_MFormatIGeneric3 extends MFormatIGeneric[EDX#Indirect]
  implicit object New_MFormatIGeneric4 extends MFormatIGeneric[EBX#Indirect]
  implicit object New_MFormatIGeneric6 extends MFormatIGeneric[ESI#Indirect]
  implicit object New_MFormatIGeneric7 extends MFormatIGeneric[EDI#Indirect]
  // There is no [EBP], its slot is used by 32-disp only mode

  implicit object New_DSFormat extends NewOneOperandFormat[DS] {
    def getAddressingForm(op1: DS, opcodeExtension: Byte) = NoAddressingForm
  }

  implicit object New_CSFormat extends NewOneOperandFormat[CS] {
    def getAddressingForm(op1: CS, opcodeExtension: Byte) = NoAddressingForm
  }

  // abbreviated  reg/imm format, used with common instrctions like 'add(eax, byte(8))'
  // implicit object New_I2Format extends NewTwoOperandFormat[reg, imm] {
  //    def getAddressingForm(op1: reg, op2: imm, opcodeExtension: Byte) = InstructionFormat (NoModRM(), Some(op2))
  //    def size = 1
  //}

  implicit object New_IFormat8 extends NewOneOperandFormat[imm8] {

    def getAddressingForm(operand: imm8, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = NoModRM(),
        immediate = Some(operand))
    }
  }

  implicit object New_IFormat16 extends NewOneOperandFormat[imm16] {

    def getAddressingForm(operand: imm16, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = NoModRM(),
        immediate = Some(operand))
    }
  }

  implicit object New_IFormat64 extends NewOneOperandFormat[imm64] {

    def getAddressingForm(operand: imm64, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = NoModRM(),
        immediate = Some(operand))
    }
  }

  implicit object New_OffsetFormat extends NewOneOperandFormat[r64 + _] {

    def getAddressingForm(operand: r64 + _, opcodeExtension: Byte) = {
      InstructionFormat(
        WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcodeExtension, operand.base), ScaleIndexByte(SIB.One, new ESP, operand.base)),
        immediate = None)
    }
  }

  implicit object New_OffsetFormat2 extends NewOneOperandFormat[reg + _8] {

    def getAddressingForm(operand: reg + _8, opcodeExtension: Byte) = {
      InstructionFormat(
        NoSIBWithDisplacement(ModRMOpcode(DisplacementByte, opcodeExtension, operand.base), operand.displacement),
        immediate = None)
    }
  }

  implicit object New_MRFormat extends NewTwoOperandFormat[reg + _8, reg] {

    def getAddressingForm(op1: reg + _8, op2: reg, opcodeExtension: Byte) = {
      New_RMFormat.getAddressingForm(op2, op1, opcodeExtension)
    }
  }

  implicit object New_RMFormatB1 extends NewTwoOperandFormat[reg, StackPointer[_] + _8] {
    def getAddressingForm(op1: reg, op2: StackPointer[_] + _8, opcodeExtension: Byte) = {
      InstructionFormat(
        WithSIBWithDisplacement(ModRMReg(DisplacementByte, op1, op2.base), ScaleIndexByte(SIB.One, new ESP, op2.base), op2.displacement),
        immediate = None)
    }
  }

  implicit object New_MIFormat8 extends NewTwoOperandFormat[reg, imm8] {

    def getAddressingForm(op1: reg, op2: imm8, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtension, op1)),
        immediate = Some(op2))
    }
  }

  implicit object New_MIFormat16 extends NewTwoOperandFormat[reg, imm16] {

    def getAddressingForm(op1: reg, op2: imm16, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtension, op1)),
        immediate = Some(op2))
    }
  }

  implicit object New_MIFormat32 extends NewTwoOperandFormat[reg, imm32] {

    def getAddressingForm(op1: reg, op2: imm32, opcodeExtension: Byte) = {
      if (opcodeExtension != -1) {
        InstructionFormat(
          addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtension, op1)),
          immediate = Some(op2))
      } else
        InstructionFormat(
          addressingForm = NoModRM(),
          immediate = Some(op2))
    }

    override def getPrefix(prefix: Seq[Prefix]) = {
      if (prefix.exists(_.isInstanceOf[REX])) {
        REX(true, false, false, true).get
      } else {
        Array[Byte]()
      }
    }
  }

  //  implicit object New_MIFormat64 extends NewTwoOperandFormat[reg, imm64] {
  //
  //    def getAddressingForm(op1: reg, op2: imm64, opcodeExtension: Byte) = {
  //      InstructionFormat(
  //        addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtension, op1)),
  //        immediate = Some(op2))
  //    }
  //  }

  implicit object New_OIFormat64 extends NewTwoOperandFormat[rm, imm64] {

    def getAddressingForm(op1: rm, op2: imm64, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = NoModRM(),
        immediate = Some(op2))
    }
  }

  implicit object New_OIFormat16 extends NewTwoOperandFormat[rm, imm16] {

    def getAddressingForm(op1: rm, op2: imm16, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = NoModRM(),
        immediate = Some(op2))
    }
  }

  //  implicit object New_OIFormat8 extends NewTwoOperandFormat[rm, imm8] {
  //
  //      def getAddressingForm(op1: rm, op2: imm8, opcodeExtension: Byte) = {
  //        InstructionFormat (
  //          addressingForm = NoModRM(),
  //          immediate = Some(op2)
  //        )
  //      }
  //  
  //      def size = 1
  //  }

  //   implicit object New_MFormat2R2 extends NewOneOperandFormat[GeneralPurpose[_64]#Indirect] {
  //
  //    def getAddressingForm(operand: GeneralPurpose[_64]#Indirect, opcodeExtension: Byte) = {
  //      InstructionFormat(
  //        addressingForm = OnlyModRM(ModRMOpcode(NoDisplacement, opcodeExtension, operand.base)), //mem.encode(opcode.opcodeExtension),
  //        immediate = None)
  //    }
  //  }

  implicit object New_MFormat2R3 extends NewOneOperandFormat[RSP#Indirect] {

    def getAddressingForm(operand: RSP#Indirect, opcodeExtension: Byte) = {
      InstructionFormat(
        WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcodeExtension, operand.base), ScaleIndexByte(SIB.One, new ESP, operand.base)),
        immediate = None)
    }
  }

  implicit object New_MFormat4 extends NewOneOperandFormat[reg] {

    def getAddressingForm(operand: reg, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtension, operand)), ///reg.encode(opcode.opcodeExtension),
        immediate = None)
    }
  }

  implicit object New_MFormatB1 extends NewOneOperandFormat[r64 + _] {

    def getAddressingForm(operand: r64 + _, opcodeExtension: Byte) = {
      InstructionFormat(
        WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcodeExtension, operand.base), ScaleIndexByte(SIB.One, new ESP, operand.base)),
        immediate = None)
    }

  }

  implicit object New_RMFormatB2 extends NewTwoOperandFormat[reg, reg + _32] {

    def getAddressingForm(op1: reg, op2: reg + _32, opcodeExtension: Byte) = {
      InstructionFormat(
        NoSIBWithDisplacement(ModRMReg(DisplacementDword, reg = op1, rm = op2.base), op2.displacement),
        immediate = None)
    }
  }

  implicit object New_MFormat5 extends NewOneOperandFormat[AbsoluteAddress[_32]] {

    def getAddressingForm(operand: AbsoluteAddress[_32], opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = NoSIBWithDisplacement(ModRMOpcode(NoDisplacement, opcodeExtension, new EBP), operand.displacement), //mem.encode(opcode.opcodeExtension),
        immediate = None)
    }
  }

  //  implicit object New_AbsoluteAddress32 extends AbsoluteAddress[_32] {
  //    selff =>
  //    var offset = 0
  //    def displacement = Constant32(offset)
  //
  //    def getRelative = new Relative[_32] {
  //      def displacement = Constant32(offset)
  //      def size = 4
  //    }
  //  }
  //
  //  implicit object New_AbsoluteAddress64 extends AbsoluteAddress[_64] {
  //    selff =>
  //    var offset: Long = 0
  //    def displacement = Constant64(offset)
  //
  //    def getRelative = new Relative[_64] {
  //      def displacement = Constant64(offset)
  //      def size = 4
  //    }
  //  }

  implicit object New_RMFormat3 extends NewTwoOperandFormat[reg, r32#Indirect] {

    def getAddressingForm(op1: reg, op2: r32#Indirect, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = OnlyModRM(ModRMReg(NoDisplacement, op1, rm = op2.base)),
        immediate = None)
    }
  }

  implicit object New_RMFormat6 extends NewTwoOperandFormat[reg, reg] {

    def getAddressingForm(op1: reg, op2: reg, opcodeExtension: Byte) = {

      InstructionFormat(
        OnlyModRM(ModRMReg(TwoRegisters, op2, op1)),
        immediate = None)
    }
  }

  implicit object New_RMFormat2 extends NewTwoOperandFormat[reg, AbsoluteAddress[_32]] {

    def getAddressingForm(op1: reg, op2: AbsoluteAddress[_32], opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = NoSIBWithDisplacement(ModRMReg(NoDisplacement, op1, new EBP), op2.displacement),
        immediate = None)
    }

    override def getPrefix(prefix: Seq[Prefix]) = {
      if (prefix.exists(_.isInstanceOf[REX])) {
        REX(true, false, false, false).get
      } else {
        Array[Byte]()
      }
    }
  }
}