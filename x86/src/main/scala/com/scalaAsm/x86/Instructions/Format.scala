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

class NoOperandFormat extends ResolvedZeroOperand(null, Seq()) {
  val size = 1
}


abstract class TwoOperandFormat[-X, -Y] {
  def getAddressingForm(op1: X, op2: Y): InstructionFormat
  def getPrefix(prefix: Seq[Prefix], op1: X, op2: Y): Array[Byte] = prefix.map(_.get).foldLeft(Array[Byte]()){ _ ++ _ }
}

abstract class OneOperandFormat[-X] {
  def getAddressingForm(op1: X): InstructionFormat
  def getPrefix(prefix: Seq[Prefix], op1: X): Array[Byte] = prefix.map(_.get).foldLeft(Array[Byte]()){ _ ++ _ }
}

abstract class ResolvedZeroOperand(opcode: OpcodeFormat, prefix: Seq[Prefix]) {
  def getPrefix: Array[Byte] = Array()
  def size: Int
}

// Instruction Format:
/*------------------------------------------------------------------------------------------------------------------------------------------\
|*Prefixes | *Mandatory Prefix | *REX Prefix | Opcode Bytes | *ModR/M | *SIB | *Displacement (1,2 or 4 bytes) | *Immediate (1,2 or 4 bytes) |
\------------------------------------------------------------------------------------------------------------------------------------------*/

trait OneOperandFormats[-X] {
  self: InstructionDefinition#OneOp[X] =>

  object ImmFormat extends OneOperandFormat[imm] {
  
    def getAddressingForm(operand: imm) = {
      InstructionFormat(
        addressingForm = NoModRM(),
        immediate = operand.getBytes)
    }
  }
  
  object RmFormat extends OneOperandFormat[rm] {
  
    def getAddressingForm(operand: rm) = {
  
      getRegFormat(operand).getOrElse{
        operand match {
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
  }
  
  def getRegFormat(regOrMemory: rm): Option[InstructionFormat] = regOrMemory match {
    case reg @ GeneralPurpose(_) if opcodeSelectsRegister =>
      Some(NoAddressingForm)
    case reg @ GeneralPurpose(_) if reg.isInstanceOf[SegmentRegister] =>
      Some(NoAddressingForm)
    case reg @ GeneralPurpose(_) =>
      Some(InstructionFormat(
        addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtension, reg)),
        immediate = Array()))
    case _ => None
  }
  
  object RegFormat extends OneOperandFormat[reg] {
  
    def getAddressingForm(operand: reg) = {
      getRegFormat(operand).get
    }
  }
}

trait TwoOperandFormats[-X,-Y] {
  self: InstructionDefinition#TwoOp[X,Y] =>

  object RmImmFormat extends TwoOperandFormat[rm, imm] {
  
    def getAddressingForm(op1: rm, op2: imm) = {
      if (opcodeExtension != -1) {
        InstructionFormat(
          addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtension, op1.asInstanceOf[reg])), // hack!
          immediate = op2.getBytes)
      } else
        InstructionFormat(
          addressingForm = NoModRM(),
          immediate = op2.getBytes)
    }
  
    override def getPrefix(prefix: Seq[Prefix], op1: rm, op2: imm) = {
      if (prefix.exists(_.isInstanceOf[REX])) {
        if (op1.asInstanceOf[reg].name == "rsp") {
          REX(true, false, false, false).get
        } else {
          REX(true, false, false, true).get
        }
      } else {
        Array[Byte]()
      }
    }
  }
  
  object RegRmFormat extends TwoOperandFormat[reg, rm] {
  
    def getAddressingForm(op1: reg, op2: rm) = {
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
  
  object MemRegFormat extends TwoOperandFormat[rm, reg] {
  
    def getAddressingForm(op1: rm, op2: reg) = {
      RegRmFormat.getAddressingForm(op2, op1)
    }
  }
}