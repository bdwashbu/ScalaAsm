package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands.TwoOperandFormat
import com.scalaAsm.x86.Operands.OneOperandFormat
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.NoOperandFormat
import com.scalaAsm.x86.Operands.Constant
import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.OpcodePlus
import com.scalaAsm.x86.Operands.Memory.ModRM

trait InstructionField {
  def getBytes: Array[Byte]
  def size: Int
}

trait x86Instruction {
  import scala.language.implicitConversions
  val mnemonic: String
  val defaultsTo64Bit = false
  def prefix = Seq[Prefix]()
  
  implicit def toPrefixSeq(x: Prefix) = Seq(x)
  implicit def toByte(x: Int) = x.toByte
  implicit def toOneOpcode(x: Int): OneOpcode = OneOpcode(x.toByte)
  implicit def toTwoOpcodes(x: (Int, Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte)
}

abstract case class ZeroMachineCode(format: ResolvedZeroOperand) extends InstructionResult {

  def line = mnemonic
  def opcode: OpcodeFormat
  def mnemonic: String

  def getSize: Int = {
    format.getPrefix.size + opcode.size
  }

  def getBytes: Array[Byte] = {
    format.getPrefix ++: opcode.get
  }
}

abstract case class OneMachineCode[O1, OpEn <: OneOperandEncoding[O1]](operand: Operand[O1], format: OneOperandFormat[O1, OpEn]) extends InstructionResult {

  val line = mnemonic
  val opcode: OpcodeFormat
  val mnemonic: String
  val prefix: Seq[Prefix]

  def getSize: Int = {
    format.size(opcode, prefix)
  }

  def getBytes: Array[Byte] = {
    if (opcode.isInstanceOf[OpcodePlus] && operand.get.isInstanceOf[ModRM.reg]) { // this is hacky as hell!
      opcode.asInstanceOf[OpcodePlus].reg = operand.get.asInstanceOf[ModRM.reg]
    }
    format.getPrefix(prefix) ++: opcode.get ++: format.getAddressingForm(operand.get, opcode).getBytes
  }
}

abstract case class TwoMachineCode[O1, O2, OpEn <: TwoOperandEncoding[O1,O2]](operand: Operand[O1], operand2: Operand[O2], format: TwoOperandFormat[O1, O2, OpEn]) extends InstructionResult {

  val line = mnemonic
  val opcode: OpcodeFormat
  val mnemonic: String
  val prefix: Seq[Prefix]

  def getSize: Int = {
    format.size(opcode, prefix)
  }

  def getBytes: Array[Byte] = {
    if (opcode.isOpcodePlus && operand.get.isInstanceOf[ModRM.reg]) { // this is even more hacky as hell!
      opcode.asInstanceOf[OpcodePlus].reg = operand.get.asInstanceOf[ModRM.reg]
    }
    format.getPrefix(prefix) ++: opcode.get ++: format.getAddressingForm(operand.get, operand2.get, opcode).getBytes
  }
}

abstract class InstructionDefinition[Opcode <: OpcodeFormat](val mnemonic: String) {
  self =>

  abstract class _0 extends x86Instruction {
    self2 =>
    def opcode: Opcode
    val mnemonic = InstructionDefinition.this.mnemonic
    def get = new ZeroMachineCode(new NoOperandFormat {}) {
      val opcode = self2.opcode
      val mnemonic = InstructionDefinition.this.mnemonic
    }
  }
  
  abstract class _1[-O1, OpEn <: OneOperandEncoding[O1]]  extends x86Instruction {
    self2 =>
    val mnemonic = InstructionDefinition.this.mnemonic
    def opcode: Opcode
    
    def apply[X <: O1](p1: Operand[X], format: OneOperandFormat[X, OpEn], prefix: Seq[Prefix]) = {
      val resolvedPrefix: Seq[Prefix] = if (defaultsTo64Bit) Seq() else prefix
      new OneMachineCode(p1, format) {
        val opcode = self2.opcode
        val mnemonic = InstructionDefinition.this.mnemonic
        val prefix = resolvedPrefix
      }
    }
  }
  
  abstract class _2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]]  extends x86Instruction {
    self2 =>
    val mnemonic = InstructionDefinition.this.mnemonic
    def opcode: Opcode
    
    def apply[X <: O1, Y <: O2](p1: Operand[X], p2: Operand[Y], format: TwoOperandFormat[X, Y, OpEn], prefix: Seq[Prefix]) = {
      val resolvedPrefix: Seq[Prefix] = if (defaultsTo64Bit) Seq() else prefix
      new TwoMachineCode(p1, p2, format) {
        val opcode = self2.opcode
        val mnemonic = InstructionDefinition.this.mnemonic
        val prefix = resolvedPrefix
      }
    }
  }
}
