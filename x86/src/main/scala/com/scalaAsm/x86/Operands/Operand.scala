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

trait One extends Operand {
  def size = 1
}

trait Constant[Self] extends InstructionField with Operand {
  def value: Size#primitiveType
  def getBytes: Array[Byte]
  def asInt: Int
  def asLong: Long
  def negate: Self
}

case class Constant8(value: Byte) extends Constant[Constant8] {
  self =>
  type Size = ByteOperand
  def getBytes: Array[Byte] = Array(value)
  def size = 1
  def asInt = value.toInt
  def asLong = value.toLong
  def negate = this.copy(value = (-this.value).toByte)
}

case class Constant16(value: Short) extends Constant[Constant16] {
  self =>
  type Size = WordOperand
  def getBytes: Array[Byte] = Array((value & 0x00FF).toByte, ((value & 0xFF00) >> 8).toByte)
  def size = 2
  def asInt = value.toInt
  def asLong = value.toLong
  def negate = this.copy(value = (-this.value).toShort)
}

case class Constant32(value: Int) extends Constant[Constant32] {
  self =>
  type Size = DwordOperand
  def getBytes: Array[Byte] = Array((value & 0x000000FF).toByte, ((value & 0x0000FF00) >> 8).toByte, ((value & 0x00FF0000) >> 16).toByte, ((value & 0xFF000000) >> 24).toByte)
  def size = 4
  def asInt = value.toInt
  def asLong = value.toLong
  def negate = this.copy(value = -this.value)
}

case class Constant64(value: Long) extends Constant[Constant64] {
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
  def negate: Constant64 = this.copy(value = -this.value)
}

sealed trait OperandSize {
  type primitiveType
}

class ByteOperand extends OperandSize { type primitiveType = Byte }
class WordOperand extends OperandSize { type primitiveType = Short }
class DwordOperand extends OperandSize { type primitiveType = Int }
class QwordOperand extends OperandSize { type primitiveType = Long }

trait RegisterOrMemory extends Operand



