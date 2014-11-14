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

trait Instruction

trait SizedInstructionField {
  def size: Int
}

trait InstructionField extends SizedInstructionField {
  def getBytes: Array[Byte]
}

trait x86Instruction extends Instruction {
  import scala.language.implicitConversions
  val mnemonic: String
  val defaultsTo64Bit = false
  def prefix = Seq[Prefix]()
  
  implicit def toPrefixSeq(x: Prefix) = Seq(x)
  implicit def toByte(x: Int) = x.toByte
  implicit def toOneOpcode(x: Int): OneOpcode = OneOpcode(x.toByte)
  implicit def toTwoOpcodes(x: (Int, Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte)
}

abstract case class ZeroMachineCode[Opcode <: OpcodeFormat]() extends InstructionResult {

  def line = mnemonic
  def opcode: Opcode
  def mnemonic: String
  def format: ResolvedZeroOperand

  def getSize: Int = {
    format.getPrefix.size + opcode.size
  }

  def getBytes: Array[Byte] = {
    format.getPrefix ++: opcode.get
  }
}

abstract case class OneMachineCode[O1, OpEn <: OneOperandEncoding[O1], Opcode <: OpcodeFormat](operand: Operand[O1]) extends InstructionResult {

  val line = mnemonic
  val opcode: Opcode
  val mnemonic: String
  def format: OneOperandFormat[O1, OpEn]
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

abstract case class TwoMachineCode[O1, O2, OpEn <: TwoOperandEncoding[O1,O2], Opcode <: OpcodeFormat](operand: Operand[O1], operand2: Operand[O2]) extends InstructionResult {

  val line = mnemonic
  val opcode: Opcode
  val mnemonic: String
  def format: TwoOperandFormat[O1, O2, OpEn]
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

//abstract class ZeroOperandInstruction[Opcode <: OpcodeFormat](val mnemonic: String) extends x86Instruction with Formats {
//  self =>
//  def opcode: Opcode
//  def get = new ZeroMachineCode[Opcode] {
//    val opcode = self.opcode
//    val mnemonic = self.mnemonic
//    def format = new NoOperandFormat {}
//  }
//}

//abstract class OneOperandInstruction[-O1, -OpEn <: OneOperandEncoding[O1], Opcode <: OpcodeFormat](val mnemonic: String) extends x86Instruction with Formats {
//  self =>
//  def opcode: Opcode
//  def apply[X, OpEn2 <: OneOperandEncoding[X]](p1: Operand[X], format2: OneOperandFormat[X, OpEn2], prefix: Seq[Prefix]) = {
//    val resolvedPrefix: Seq[Prefix] = if (defaultsTo64Bit) Seq() else prefix
//
//    new OneMachineCode[X,OpEn2, Opcode](p1) {
//      val opcode = self.opcode
//      val mnemonic = self.mnemonic
//      def format = format2
//      val prefix = resolvedPrefix
//    }
//  }
//}

abstract class OperandInstruction[Opcode <: OpcodeFormat](val mnemonic: String) {
  self =>
  
    
  
  abstract class ZeroOps extends x86Instruction with Formats {
    self2 =>
    def opcode: Opcode
    val mnemonic = self.mnemonic
    def get = new ZeroMachineCode[Opcode] {
      val opcode = self2.opcode
      val mnemonic = self.mnemonic
      def format = new NoOperandFormat {}
    }
  }
  
  abstract class TwoOps[-O1, -O2, -OpEn <: TwoOperandEncoding[O1, O2]]  extends x86Instruction with Formats {
    self2 =>
    val mnemonic = self.mnemonic
    def opcode: Opcode
    def apply[X, Y, OpEn2 <: TwoOperandEncoding[X, Y]](p1: Operand[X], p2: Operand[Y], format2: TwoOperandFormat[X, Y, OpEn2], prefix: Seq[Prefix]) = {
      val resolvedPrefix: Seq[Prefix] = if (defaultsTo64Bit) Seq() else prefix
      new TwoMachineCode[X, Y, OpEn2, Opcode](p1, p2) {
        val opcode = self2.opcode
        val mnemonic = self.mnemonic
        def format = format2
        val prefix = resolvedPrefix
      }
    }
  }
  
  abstract class OneOp[-O1, -OpEn <: OneOperandEncoding[O1]]  extends x86Instruction with Formats {
    self2 =>
    val mnemonic = self.mnemonic
    def opcode: Opcode
    def apply[X, OpEn2 <: OneOperandEncoding[X]](p1: Operand[X], format2: OneOperandFormat[X, OpEn2], prefix: Seq[Prefix]) = {
      val resolvedPrefix: Seq[Prefix] = if (defaultsTo64Bit) Seq() else prefix
      new OneMachineCode[X,OpEn2, Opcode](p1) {
        val opcode = self2.opcode
        val mnemonic = self.mnemonic
        def format = format2
        val prefix = resolvedPrefix
      }
    }
  }
}
