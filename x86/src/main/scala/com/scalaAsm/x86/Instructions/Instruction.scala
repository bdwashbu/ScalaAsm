package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands.TwoOperandFormat
import com.scalaAsm.x86.Operands.OneOperandFormat
import com.scalaAsm.x86.Operands.Constant8
import com.scalaAsm.x86.Operands.NoOperandFormat
import com.scalaAsm.x86.Operands.Constant

trait Instruction

trait SizedInstructionField {
  def size: Int
}

trait InstructionField extends SizedInstructionField {
  def getBytes: Array[Byte]
}

trait x86Instruction extends Instruction {
  import scala.language.implicitConversions
  val opcode: OpcodeFormat
  val mnemonic: String
  implicit def toByte(x: Int) = x.toByte
  implicit def toOneOpcode(x: Int): OneOpcode = OneOpcode(x.toByte)
  implicit def toTwoOpcodes(x: (Int, Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte)
}

trait OperandEncoding {
  type Immediate = Constant[_]
}

trait MachineCodeBuilder {
  def get: MachineCode
}

class OneMachineCodeBuilder[O1 <: Operand, X](operand: O1, opcode: OpcodeFormat, mnemonic: String, format: OneOperandFormat[X, O1]) extends MachineCodeBuilder {
  def get() =
    new MachineCode {
        val size = getSize
        val code = getBytes
        val line = mnemonic
      }

  def getSize: Int = {
    val prefixes = format.getPrefixes(operand) getOrElse Array()
    prefixes.size + opcode.size + format.getAddressingForm(opcode)(operand).size
  }

  private def getBytes: Array[Byte] = {
    val prefixes = format.getPrefixes(operand) getOrElse Array()
    prefixes ++: opcode.get(OneOperand(operand)) ++: format.getAddressingForm(opcode)(operand).getBytes
  }
}

class TwoMachineCodeBuilder[O1 <: Operand, O2 <: Operand, X](operand: O1, operand2: O2, opcode: OpcodeFormat, mnemonic: String, format: TwoOperandFormat[X, O1, O2]) extends MachineCodeBuilder {
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

  private def getBytes: Array[Byte] = {
    val prefixes = format.getPrefixes(operand, operand2) getOrElse Array()
    prefixes ++: opcode.get(TwoOperands(operand, operand2)) ++: format.getAddressingForm(operand, operand2, opcode).getBytes
  }
}

abstract class ZeroOperandInstruction extends x86Instruction with Formats {
  def get[X] = new OneMachineCodeBuilder(Constant8(0), opcode, mnemonic, new NoOperandFormat[X, Constant8] {}) {}
}

abstract class OneOperandInstruction[OpEn, -O1 <: Operand] extends x86Instruction with Formats {
  def get[X <: O1](x: X, format: OneOperandFormat[OpEn, X]) = new OneMachineCodeBuilder(x, opcode, mnemonic, format) {}
}

abstract class TwoOperandInstruction[OpEn, -O1 <: Operand, -O2 <: Operand] extends x86Instruction with Formats {
  def get[X <: O1, Y <: O2](x: X, y:Y, format: TwoOperandFormat[OpEn, X, Y]) = new TwoMachineCodeBuilder(x, y, opcode, mnemonic, format) {}
}

abstract class TwoOperandSizedInstruction[OpEn, -O1 <: Operand, -O2 <: Operand] extends x86Instruction with Formats {
  def get[X <: O1, Y <: O2](x: X, y:Y, size: InstructionSize[X,Y], format: TwoOperandFormat[OpEn, X,Y]) = new TwoMachineCodeBuilder(x, y, opcode, mnemonic, format) {
    override def getSize: Int = size.getSize
  }
}
