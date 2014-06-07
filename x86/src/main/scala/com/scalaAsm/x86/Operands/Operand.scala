package com.scalaAsm.x86
package Operands

import java.nio.ByteBuffer
import java.nio.ByteOrder
import com.scalaAsm.x86.Operands.Memory._
import com.scalaAsm.x86.Operands.Memory.WithSIBWithDisplacement
import com.scalaAsm.x86.Operands.Memory.SIB
import com.scalaAsm.x86.Operands.Memory.NoSIBWithDisplacement
import com.scalaAsm.x86.Operands.Memory.NoModRM
import com.scalaAsm.x86.Operands.Memory.NoDisplacement
import com.scalaAsm.x86.Operands.Memory.ModRMReg
import com.scalaAsm.x86.Operands.Memory.ModRMOpcode
import com.scalaAsm.x86.Operands.Memory.DisplacementByte
import com.scalaAsm.x86.Operands.Memory.AddressingFormSpecifier

trait Operand {
  type Size <: OperandSize
}

trait Constant extends InstructionField with Operand {
  type Size <: OperandSize
  def value: Size#size
  def getBytes: Array[Byte]
  def asInt: Int
  def asLong: Long
  def negate: Constant
}

trait Constant8 extends Constant {
  self =>
  type Size = ByteOperand
  def getBytes: Array[Byte] = Array(value)
  def size = 1
  def asInt = value.toInt
  def asLong = value.toLong
  def negate: Constant8 = new Constant8 {
	  override def negate: Constant8 = self
	  val value = (-self.value).toByte
  }
}

trait Constant16 extends Constant {
  self =>
  type Size = WordOperand
  def getBytes: Array[Byte] = Array((value & 0x00FF).toByte, ((value & 0xFF00) >> 8).toByte)
  def size = 2
  def asInt = value.toInt
  def asLong = value.toLong
  def negate: Constant16 = new Constant16 {
	  override def negate: Constant16 = self
	  val value: Short = (-self.value).toShort
  }
}

trait Constant32 extends Constant {
  self =>
  type Size = DwordOperand
  def getBytes: Array[Byte] = Array((value & 0x000000FF).toByte, ((value & 0x0000FF00) >> 8).toByte, ((value & 0x00FF0000) >> 16).toByte, ((value & 0xFF000000) >> 24).toByte)
  def size = 4
  def asInt = value.toInt
  def asLong = value.toLong
  def negate: Constant32 = new Constant32{
	  override def negate: Constant32 = self
	  val value: Int = -self.value
  }
}

trait Constant64 extends Constant {
  self =>
  type Size = QwordOperand
  def getBytes: Array[Byte] = {
    val buffer = ByteBuffer.allocate(8)
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      buffer.putLong(value)
      buffer.array()
  }
  def size = 8
  def asInt = value.toInt
  def asLong = value.toLong
  def negate: Constant64 = new Constant64{
	  override def negate: Constant64 = self
	  val value: Long = -self.value
  }
}

sealed trait OperandSize {
  type size
}

class ByteOperand extends OperandSize { type size = Byte }
class WordOperand extends OperandSize { type size = Short }
class DwordOperand extends OperandSize { type size = Int }
class QwordOperand extends OperandSize { type size = Long }

trait RegisterOrMemory extends Operand {
  type Size <: OperandSize
}



