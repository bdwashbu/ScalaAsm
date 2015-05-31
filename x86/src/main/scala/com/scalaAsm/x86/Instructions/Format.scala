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

trait Formats {

  implicit object ImmFormat extends OneOperandFormat[imm] {

    def getAddressingForm(operand: imm, opcodeExtension: Byte, opcodeSelectsRegister: Boolean) = {
      InstructionFormat(
        addressingForm = NoModRM(),
        immediate = operand.getBytes)
    }
  }

  implicit object RmFormat extends OneOperandFormat[rm] {

    def getAddressingForm(operand: rm, opcodeExtension: Byte, opcodeSelectsRegister: Boolean) = {
      
      operand match {
        case reg @ GeneralPurpose(_) if opcodeSelectsRegister =>
          NoAddressingForm
        case reg @ GeneralPurpose(_) if reg.isInstanceOf[SegmentRegister] => 
          NoAddressingForm
        case reg @ GeneralPurpose(_) =>
          InstructionFormat(
            addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtension, reg)),
            immediate = Array())
        case BaseIndex(base, offset) if (base.name == "rsp" || base.name == "esp") => 
            InstructionFormat(
            WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcodeExtension, base), ScaleIndexByte(SIB.One, new ESP, base)),
            immediate = Array()) 
        case BaseIndex(base, offset) =>
            InstructionFormat(
              NoSIBWithDisplacement(ModRMOpcode(DisplacementByte, opcodeExtension, base), offset.getBytes),
              immediate = Array())
        case Indirect(base) if (base.name == "rsp" || base.name == "esp") =>
             InstructionFormat(
            WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcodeExtension, base), ScaleIndexByte(SIB.One, new ESP, base)),
            immediate = Array())
        case Indirect(base) =>
            InstructionFormat(
              addressingForm = OnlyModRM(ModRMOpcode(NoDisplacement, opcodeExtension, base)),
              immediate = Array())
        case x @ AbsoluteAddress(offset) =>
           InstructionFormat(
          addressingForm = NoSIBWithDisplacement(ModRMOpcode(NoDisplacement, opcodeExtension, new EBP), x.getBytes), //mem.encode(opcode.opcodeExtension),
          immediate = Array())
      }
    }
  }

  implicit object RegImmFormat extends TwoOperandFormat[reg, imm] {

    def getAddressingForm(op1: reg, op2: imm, opcodeExtension: Byte, opcodeSelectsRegister: Boolean) = {
      if (opcodeExtension != -1) {
        InstructionFormat(
          addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtension, op1)),
          immediate = op2.getBytes)
      } else
        InstructionFormat(
          addressingForm = NoModRM(),
          immediate = op2.getBytes)
    }

    override def getPrefix(prefix: Seq[Prefix], op1: reg, op2: imm) = {
      if (prefix.exists(_.isInstanceOf[REX])) {
        if (op1.name == "rsp") {
          REX(true, false, false, false).get
        } else {
          REX(true, false, false, true).get
        }
      } else {
        Array[Byte]()
      }
    }
  }

   implicit object RegRmFormat extends TwoOperandFormat[reg, rm] {

    def getAddressingForm(op1: reg, op2: rm, opcodeExtension: Byte, opcodeSelectsRegister: Boolean) = {
      op2 match {
        case reg @ GeneralPurpose(_) =>
           InstructionFormat(
            OnlyModRM(ModRMReg(TwoRegisters, op1, reg)),
            immediate = Array())
        case Indirect(base) =>
          InstructionFormat(
            addressingForm = OnlyModRM(ModRMReg(NoDisplacement, op1, rm = base)),
            immediate = Array())
        case BaseIndex(base, offset) =>
          if (offset.size == 1) {
            if (base.name == "rsp" || base.name == "esp") {
              InstructionFormat(
              WithSIBWithDisplacement(ModRMReg(DisplacementByte, op1, base), ScaleIndexByte(SIB.One, new ESP, base), offset.getBytes),
              immediate = Array())
            } else {
              InstructionFormat(
                NoSIBWithDisplacement(ModRMReg(DisplacementByte, reg = op1, rm = base), offset.getBytes),
                immediate = Array())
            }
          } else {
            InstructionFormat(
            NoSIBWithDisplacement(ModRMReg(DisplacementDword, reg = op1, rm = base), offset.getBytes),
            immediate = Array())
          }
        case op2 @ AbsoluteAddress(address) =>
          InstructionFormat(
            addressingForm = NoSIBWithDisplacement(ModRMReg(NoDisplacement, op1, new EBP), op2.getBytes),
            immediate = Array())
      }  
    }
    
    override def getPrefix(prefix: Seq[Prefix], op1: reg, op2: rm) = {
      if (prefix.exists(_.isInstanceOf[REX])) {
        REX(true, false, false, false).get
      } else {
        Array[Byte]()
      }
    }
  }
   
  implicit object MemRegFormat extends TwoOperandFormat[Memory[_], reg] {

    def getAddressingForm(op1: Memory[_], op2: reg, opcodeExtension: Byte, opcodeSelectsRegister: Boolean) = {
      RegRmFormat.getAddressingForm(op2, op1, opcodeExtension, opcodeSelectsRegister)
    }
  }
}