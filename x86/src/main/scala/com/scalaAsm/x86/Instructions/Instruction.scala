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

class OneMachineCodeBuilder[O1, X](operand: O1, opcode: OpcodeFormat, mnemonic: String, format: ResolvedOneOperand[O1]) extends InstructionResult with Catalog {

  def line = mnemonic

  def getSize: Int = {
    val prefixes = format.getPrefixes(operand) getOrElse Array()
    prefixes.size + format.size
  }

  def getBytes: Array[Byte] = {
    val prefixes = format.getPrefixes(operand) getOrElse Array()
    prefixes ++: opcode.get(OneOperand(operand)) ++: format.getAddressingForm(operand).getBytes
  }
}

class TwoMachineCodeBuilder[O1, O2, X](operand: O1, operand2: O2, opcode: OpcodeFormat, mnemonic: String, format: ResolvedTwoOperands[O1, O2]) extends InstructionResult {

  def line = mnemonic

  def getSize: Int = {
    val prefixes = format.getPrefixes(operand, operand2) getOrElse Array()
    prefixes.size + format.size
  }

  def getBytes: Array[Byte] = {
    val prefixes = format.getPrefixes(operand, operand2) getOrElse Array()
    prefixes ++: opcode.get(TwoOperands(operand, operand2)) ++: format.getAddressingForm(operand, operand2).getBytes
  }
}

abstract class ZeroOperandInstruction extends x86Instruction with Formats with OperandEncoding {
  def get[X] = new OneMachineCodeBuilder(Constant8(0), opcode, mnemonic, new NoOperandFormat {}) {}
}

abstract class OneOperandInstruction[OpEn, -O1] extends x86Instruction with Formats with Catalog {
  def apply[O1: Sized, OpEn](p1: O1, format: OneOperandFormat[OpEn, O1]) = {
    val resolved = format(implicitly[Sized[O1]].size, opcode)
    new OneMachineCodeBuilder[O1,OpEn](p1, opcode, mnemonic, resolved) {}
  }
}

abstract class TwoOperandInstruction[OpEn, -O1, -O2] extends x86Instruction with Formats {
  def apply[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2, format: TwoOperandFormat[OpEn, O1,O2]) = {
    val resolved = format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, opcode)
    new TwoMachineCodeBuilder[O1,O2,OpEn](p1, p2, opcode, mnemonic, resolved) {}
  }
}
