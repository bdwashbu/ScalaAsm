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

trait MachineCodeBuilder {
  def get: MachineCode
}

class OneMachineCodeBuilder[O1, X](operand: O1, opcode: OpcodeFormat, mnemonic: String, format: ResolvedOneOperand[O1]) extends MachineCodeBuilder with Catalog {
  def get() =
    new MachineCode {
        val size = getSize
        val code = getBytes
        val line = mnemonic
      }

  def getSize: Int = {
    val prefixes = format.getPrefixes(operand) getOrElse Array()
    prefixes.size + format.size
  }

  private def getBytes: Array[Byte] = {
    val prefixes = format.getPrefixes(operand) getOrElse Array()
    prefixes ++: opcode.get(OneOperand(operand)) ++: format.getAddressingForm(operand).getBytes
  }
}

class TwoMachineCodeBuilder[O1, O2, X](operand: O1, operand2: O2, opcode: OpcodeFormat, mnemonic: String, format: ResolvedTwoOperands[O1, O2]) extends MachineCodeBuilder {
  def get() =
    new MachineCode {
        val size = getSize
        val code = getBytes
        val line = mnemonic
      }

  def getSize: Int = {
    val prefixes = format.getPrefixes(operand, operand2) getOrElse Array()
    prefixes.size + format.size
  }

  private def getBytes: Array[Byte] = {
    val prefixes = format.getPrefixes(operand, operand2) getOrElse Array()
    prefixes ++: opcode.get(TwoOperands(operand, operand2)) ++: format.getAddressingForm(operand, operand2).getBytes
  }
}

abstract class ZeroOperandInstruction extends x86Instruction with Formats with OperandEncoding {
  def get[X] = new OneMachineCodeBuilder(Constant8(0), opcode, mnemonic, new NoOperandFormat {}) {}
}

abstract class OneOperandInstruction[OpEn, -O1] extends x86Instruction with Formats with Catalog {
  def get[X <: O1](x: X, format: ResolvedOneOperand[X]) = new OneMachineCodeBuilder[X,OpEn](x, opcode, mnemonic, format) {}
}

abstract class TwoOperandInstruction[OpEn, -O1, -O2] extends x86Instruction with Formats {
  def get[X <: O1, Y <: O2](x: X, y:Y, format: ResolvedTwoOperands[X, Y]) = new TwoMachineCodeBuilder[X,Y,OpEn](x, y, opcode, mnemonic, format) {}
}
