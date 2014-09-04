package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands.TwoOperandFormat
import com.scalaAsm.x86.Operands.OneOperandFormat
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.NoOperandFormat
import com.scalaAsm.x86.Operands.Constant
import com.scalaAsm.x86.Operands.Memory.Relative

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

abstract case class OneMachineCodeBuilder[O1, OpEn <: OneOperandEncoding[O1]](operand: Operand[_,O1]) extends InstructionResult {

  def line = mnemonic
  def opcode: OpcodeFormat
  def mnemonic: String
  def format: ResolvedOneOperand[O1]

  def getSize: Int = {
    format.getPrefix.size + format.size
  }

  def getBytes: Array[Byte] = {
    format.getPrefix ++: opcode.get(OneOperand(operand)) ++: format.getAddressingForm(operand.get).getBytes
  }
}

abstract case class TwoMachineCodeBuilder[O1, O2, OpEn <: TwoOperandEncoding[O1,O2]](operand: Operand[_,O1], operand2: Operand[_,O2]) extends InstructionResult {

  def line = mnemonic
  def opcode: OpcodeFormat
  def mnemonic: String
  def format: ResolvedTwoOperands[O1, O2]

  def getSize: Int = {
    format.getPrefix.size + format.size
  }

  def getBytes: Array[Byte] = {
    format.getPrefix ++: opcode.get(TwoOperands(operand, operand2)) ++: format.getAddressingForm(operand.get, operand2.get).getBytes
  }
}

abstract class ZeroOperandInstruction extends x86Instruction with Formats with OperandEncoding {
  self =>
  def get[X] = new OneMachineCodeBuilder(Op(Constant8(0))) {
    def opcode = self.opcode
    def mnemonic = self.mnemonic
    def format = new NoOperandFormat {}
  }
}

abstract class OneOperandInstruction[-O1, -OpEn <: OneOperandEncoding[O1]] extends x86Instruction with Formats {
  self =>
  def apply[X: Sized, OpEn2 <: OneOperandEncoding[X]](p1: Operand[_, X], format: OneOperandFormat[X, OpEn2], prefix: Array[Byte]) = {
    val resolvedPrefix: Array[Byte] = if (defaultsTo64Bit) Array() else prefix
    val resolved = format(implicitly[Sized[X]].size, opcode, resolvedPrefix)
    new OneMachineCodeBuilder[X,OpEn2](p1) {
      def opcode = self.opcode
      def mnemonic = self.mnemonic
      def format = resolved
    }
  }
}

abstract class TwoOperandInstruction[-O1, -O2, -OpEn <: TwoOperandEncoding[O1, O2]] extends x86Instruction with Formats {
  self =>
  def apply[X: Sized, Y: Sized, OpEn2 <: TwoOperandEncoding[X, Y]](p1: Operand[_, X], p2: Operand[_, Y], format: TwoOperandFormat[X, Y, OpEn2], prefix: Array[Byte]) = {
    val resolved = format(implicitly[Sized[X]].size, implicitly[Sized[Y]].size, opcode, prefix)
    new TwoMachineCodeBuilder[X, Y, OpEn2](p1, p2) {
      def opcode = self.opcode
      def mnemonic = self.mnemonic
      def format = resolved
    }
  }
}
