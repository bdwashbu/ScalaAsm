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
  
  implicit def toByte(x: Int) = x.toByte
  implicit def toOneOpcode(x: Int): OneOpcode = OneOpcode(x.toByte)
  implicit def toTwoOpcodes(x: (Int, Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte)
}

trait OperandEncoding extends OperandSizes {
//  
//  implicit object blah1 extends Sized[Relative[ByteOperand]] { val size = 1 }
//  
//  
  implicit object const1 extends Sized[Constant8] { val size = 1 }
  implicit object const2 extends Sized[Constant16] { val size = 2 }
  implicit object const3 extends Sized[Constant32] { val size = 4 }
  implicit object const4 extends Sized[Constant64] { val size = 8 }
}

abstract case class ZeroMachineCodeBuilder[Opcode <: OpcodeFormat]() extends InstructionResult {

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

abstract case class OneMachineCodeBuilder[O1, OpEn <: OneOperandEncoding[O1], Opcode <: OpcodeFormat](operand: Operand[_,O1]) extends InstructionResult {

  val line = mnemonic
  val opcode: Opcode
  val mnemonic: String
  def format: ResolvedOneOperand[O1]

  def getSize: Int = {
    format.size
  }

  def getBytes: Array[Byte] = {
    if (opcode.isInstanceOf[OpcodePlus] && operand.get.isInstanceOf[ModRM.reg]) { // this is hacky as hell!
      opcode.asInstanceOf[OpcodePlus].reg = operand.get.asInstanceOf[ModRM.reg]
    }
    format.getPrefix ++: opcode.get ++: format.getAddressingForm(operand.get).getBytes
  }
}

abstract case class TwoMachineCodeBuilder[O1, O2, OpEn <: TwoOperandEncoding[O1,O2], Opcode <: OpcodeFormat](operand: Operand[_,O1], operand2: Operand[_,O2]) extends InstructionResult {

  val line = mnemonic
  val opcode: Opcode
  val mnemonic: String
  def format: ResolvedTwoOperands[O1, O2]

  def getSize: Int = {
    format.size
  }

  def getBytes: Array[Byte] = {
    if (opcode.isOpcodePlus && operand.get.isInstanceOf[ModRM.reg]) { // this is even more hacky as hell!
      opcode.asInstanceOf[OpcodePlus].reg = operand.get.asInstanceOf[ModRM.reg]
    }
    format.getPrefix ++: opcode.get ++: format.getAddressingForm(operand.get, operand2.get).getBytes
  }
}

abstract class ZeroOperandInstruction[Opcode <: OpcodeFormat] extends x86Instruction with Formats with OperandEncoding {
  self =>
  def opcode: Opcode
  def get[X] = new ZeroMachineCodeBuilder[Opcode] {
    val opcode = self.opcode
    val mnemonic = self.mnemonic
    def format = new NoOperandFormat {}
  }
}

abstract class OneOperandInstruction[-O1, -OpEn <: OneOperandEncoding[O1], Opcode <: OpcodeFormat] extends x86Instruction with Formats {
  self =>
  def opcode: Opcode
  def apply[X: Sized, OpEn2 <: OneOperandEncoding[X]](p1: Operand[_, X], format: OneOperandFormat[X, OpEn2], prefix: Array[Byte]) = {
    val resolvedPrefix: Array[Byte] = if (defaultsTo64Bit) Array() else prefix
    val resolved = format(implicitly[Sized[X]].size, opcode, resolvedPrefix)
    new OneMachineCodeBuilder[X,OpEn2, Opcode](p1) {
      val opcode = self.opcode
      val mnemonic = self.mnemonic
      def format = resolved
    }
  }
}

abstract class TwoOperandInstruction[-O1, -O2, -OpEn <: TwoOperandEncoding[O1, O2], Opcode <: OpcodeFormat] extends x86Instruction with Formats {
  self =>
  def opcode: Opcode
  
  def apply[X: Sized, Y: Sized, OpEn2 <: TwoOperandEncoding[X, Y]](p1: Operand[_, X], p2: Operand[_, Y], format: TwoOperandFormat[X, Y, OpEn2], prefix: Array[Byte]) = {
    val resolved = format(implicitly[Sized[X]].size, implicitly[Sized[Y]].size, opcode, prefix)
    new TwoMachineCodeBuilder[X, Y, OpEn2, Opcode](p1, p2) {
      val opcode = self.opcode
      val mnemonic = self.mnemonic
      def format = resolved
    }
  }
}
