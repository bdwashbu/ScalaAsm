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
import com.scalaAsm.x86.Operands.ConstantWriter

trait Low {
  implicit object New_RMFormat extends TwoOperandFormat[reg, reg#BaseIndex[_8]] {

    def getAddressingForm(op1: reg, op2: reg#BaseIndex[_8], opcodeExtension: Byte) = {
      if (op2.base.name == "rsp" || op2.base.name == "esp") {
        InstructionFormat(
        WithSIBWithDisplacement(ModRMReg(DisplacementByte, op1, op2.base), ScaleIndexByte(SIB.One, new ESP, op2.base), op2.displacement.getBytes),
        immediate = Array())
      } else {
        InstructionFormat(
          NoSIBWithDisplacement(ModRMReg(DisplacementByte, reg = op1, rm = op2.base), op2.displacement.getBytes),
          immediate = Array())
      }
    }
  }

   implicit object New_OIFormat64 extends TwoOperandFormat[rm, imm] {

    def getAddressingForm(op1: rm, op2: imm, opcodeExtension: Byte) = {
      if (op1.isInstanceOf[reg]) {
         InstructionFormat(
        addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtension, op1.asInstanceOf[reg])),
        immediate = op2.getBytes)
      } else {
        InstructionFormat(
        addressingForm = NoModRM(),
        immediate = op2.getBytes)
      }
      
     
    }
  }

}

trait Formats extends Low {

  implicit object New_MFormat64 extends OneOperandFormat[rel64] {

    def getAddressingForm(operand: rel64, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = OnlyDisplacement(operand.getBytes),
        immediate = Array())
    }
  }

  implicit object New_DSFormat extends OneOperandFormat[DS] {
    def getAddressingForm(op1: DS, opcodeExtension: Byte) = NoAddressingForm
  }

  implicit object New_CSFormat extends OneOperandFormat[CS] {
    def getAddressingForm(op1: CS, opcodeExtension: Byte) = NoAddressingForm
  }

  implicit object New_IFormat8 extends OneOperandFormat[imm] {

    def getAddressingForm(operand: imm, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = NoModRM(),
        immediate = operand.getBytes)
    }
  }

  implicit object New_OffsetFormat2 extends OneOperandFormat[reg#BaseIndex[_8]] {

    def getAddressingForm(operand: reg#BaseIndex[_8], opcodeExtension: Byte) = {
      if (operand.base.name == "rsp" || operand.base.name == "esp") {     
        InstructionFormat(
        WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcodeExtension, operand.base), ScaleIndexByte(SIB.One, new ESP, operand.base)),
        immediate = Array()) 
      } else {
        InstructionFormat(
          NoSIBWithDisplacement(ModRMOpcode(DisplacementByte, opcodeExtension, operand.base), operand.displacement.getBytes),
          immediate = Array())
      }
    }
  }

  implicit object New_MRFormat extends TwoOperandFormat[reg#BaseIndex[_8], reg] {

    def getAddressingForm(op1: reg#BaseIndex[_8], op2: reg, opcodeExtension: Byte) = {
      New_RMFormat.getAddressingForm(op2, op1, opcodeExtension)
    }
  }

  implicit object New_MIFormat32 extends TwoOperandFormat[reg, imm32] {

    def getAddressingForm(op1: reg, op2: imm32, opcodeExtension: Byte) = {
      if (opcodeExtension != -1) {
        InstructionFormat(
          addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtension, op1)),
          immediate = op2.getBytes)
      } else
        InstructionFormat(
          addressingForm = NoModRM(),
          immediate = op2.getBytes)
    }

    override def getPrefix(prefix: Seq[Prefix]) = {
      if (prefix.exists(_.isInstanceOf[REX])) {
        REX(true, false, false, true).get
      } else {
        Array[Byte]()
      }
    }
  }

  implicit object MFormatIGeneric extends OneOperandFormat[Indirect[_]] {

    def getAddressingForm(operand: Indirect[_], opcodeExtension: Byte) = {
      if (Seq("rsp", "esp").contains(operand.base.name)) {
         InstructionFormat(
        WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcodeExtension, operand.base), ScaleIndexByte(SIB.One, new ESP, operand.base)),
        immediate = Array())
      } else {
      InstructionFormat(
        addressingForm = OnlyModRM(ModRMOpcode(NoDisplacement, opcodeExtension, operand.base)),
        immediate = Array())
      }
    }
  }

   implicit object New_RMFormat3 extends TwoOperandFormat[reg, Indirect[_32]] {

    def getAddressingForm(op1: reg, op2: Indirect[_32], opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = OnlyModRM(ModRMReg(NoDisplacement, op1, rm = op2.base)),
        immediate = Array())
    }
  }
  
  implicit object New_MFormat4 extends OneOperandFormat[reg] {

    def getAddressingForm(operand: reg, opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtension, operand)), ///reg.encode(opcode.opcodeExtension),
        immediate = Array())
    }
  }

  implicit object New_MFormatB1 extends OneOperandFormat[r64#BaseIndex[_]] {

    def getAddressingForm(operand: r64#BaseIndex[_], opcodeExtension: Byte) = {
      InstructionFormat(
        WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcodeExtension, operand.base), ScaleIndexByte(SIB.One, new ESP, operand.base)),
        immediate = Array())
    }

  }

  implicit object New_RMFormatB2 extends TwoOperandFormat[reg, reg#BaseIndex[_32]] {

    def getAddressingForm(op1: reg, op2: reg#BaseIndex[_32], opcodeExtension: Byte) = {
      InstructionFormat(
        NoSIBWithDisplacement(ModRMReg(DisplacementDword, reg = op1, rm = op2.base), op2.displacement.getBytes),
        immediate = Array())
    }
  }

  implicit object New_MFormat5 extends OneOperandFormat[AbsoluteAddress[_]] {

    def getAddressingForm(operand: AbsoluteAddress[_], opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = NoSIBWithDisplacement(ModRMOpcode(NoDisplacement, opcodeExtension, new EBP), operand.getBytes), //mem.encode(opcode.opcodeExtension),
        immediate = Array())
    }
  }

  implicit object New_RMFormat6 extends TwoOperandFormat[reg, reg] {

    def getAddressingForm(op1: reg, op2: reg, opcodeExtension: Byte) = {

      InstructionFormat(
        OnlyModRM(ModRMReg(TwoRegisters, op2, op1)),
        immediate = Array())
    }
  }

  implicit object New_RMFormat2 extends TwoOperandFormat[reg, AbsoluteAddress[_]] {

    def getAddressingForm(op1: reg, op2: AbsoluteAddress[_], opcodeExtension: Byte) = {
      InstructionFormat(
        addressingForm = NoSIBWithDisplacement(ModRMReg(NoDisplacement, op1, new EBP), op2.getBytes),
        immediate = Array())
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