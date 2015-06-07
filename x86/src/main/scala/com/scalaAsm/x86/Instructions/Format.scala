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

object NoAddressingForm extends InstructionFormat(addressingForm = NoModRM(), immediate = Array())

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
        val result = operand match {
          case BaseIndex(base, offset) if (base.name == "rsp" || base.name == "esp") =>
              WithSIBNoDisplacement(ModRMOpcode(Indirect0, opcodeExtension, base.ID), ScaleIndexByte(SIB.One, new ESP, base))
          case BaseIndex(base, offset) =>
              NoSIBWithDisplacement(ModRMOpcode(Indirect8, opcodeExtension, base.ID), offset.getBytes)
          case Indirect(base) if (base.name == "rsp" || base.name == "esp") =>
              WithSIBNoDisplacement(ModRMOpcode(Indirect0, opcodeExtension, base.ID), ScaleIndexByte(SIB.One, new ESP, base))
          case Indirect(base) =>
              OnlyModRM(ModRMOpcode(Indirect0, opcodeExtension, base.ID))
          case x @ AbsoluteAddress(offset, _) =>
              NoSIBWithDisplacement(ModRMOpcode(Indirect0, opcodeExtension, (new EBP).ID), x.getBytes)
        }
        
        InstructionFormat(result, Array())
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
        addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtension, reg.ID)),
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
          addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtension, op1.asInstanceOf[reg].ID)), // hack!
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
      val result = op2 match {
        case reg @ GeneralPurpose(_) =>
            OnlyModRM(ModRMReg(TwoRegisters, op1.ID, reg.ID))
        case Indirect(base) =>
            OnlyModRM(ModRMReg(Indirect0, op1.ID, rm = base.ID))
        case BaseIndex(base, offset) =>
          if (offset.size == 1) {
            if (base.name == "rsp" || base.name == "esp") {
                WithSIBWithDisplacement(ModRMReg(Indirect8, op1.ID, base.ID), ScaleIndexByte(SIB.One, new ESP, base), offset.getBytes)
            } else {
                NoSIBWithDisplacement(ModRMReg(Indirect8, op1.ID, base.ID), offset.getBytes)
            }
          } else {
              NoSIBWithDisplacement(ModRMReg(Indirect32, op1.ID, base.ID), offset.getBytes)
          }
        case op2 @ AbsoluteAddress(address, _) =>
            NoSIBWithDisplacement(ModRMReg(Indirect0, op1.ID, (new EBP).ID), op2.getBytes)
      }
      
      InstructionFormat(result, Array())
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