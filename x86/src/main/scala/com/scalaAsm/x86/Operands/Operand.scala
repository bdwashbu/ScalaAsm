package com.scalaAsm.x86
package Operands

import com.scalaAsm.x86.Instructions.InstructionField
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress

trait One extends Operand[One,One]{
  def size = 1
  def get = this
}

case class addr(name: String) extends Operand[String, AbsoluteAddress[_32]] {
  var variables: Map[String, Int] = _
  var baseOffset: Int = _
  var parserPosition: Int = 0
  def get = new AbsoluteAddress[_32] {
    var offset = variables(name)
    def getRelative = null
    def displacement = Constant32(variables(name) - 0x2000 - parserPosition - 7)
  }
}

trait Operand[+From, +To] {
  def get: To
}

case class Op[X](from: X) extends Operand[X, X] { def get = from }

trait Constant[Size <: OperandSize] extends InstructionField {
  def value: Size#primitiveType
  def getBytes: Array[Byte]
  def asInt: Int
  def asLong: Long
  def negate: Constant[Size]
  def size: Int
}

case class Constant8(value: Byte) extends Constant[_8] {
  self =>
  def getBytes: Array[Byte] = Array(value)
  def size = 1
  def asInt = value.toInt
  def asLong = value.toLong
  def negate = this.copy(value = (-this.value).toByte)
}

case class Constant16(value: Short) extends Constant[_16] {
  self =>
  def getBytes: Array[Byte] = Array((value & 0x00FF).toByte, ((value & 0xFF00) >> 8).toByte)
  def size = 2
  def asInt = value.toInt
  def asLong = value.toLong
  def negate = this.copy(value = (-this.value).toShort)
}

case class Constant32(value: Int) extends Constant[_32] {
  self =>
  def getBytes: Array[Byte] = Array((value & 0x000000FF).toByte, ((value & 0x0000FF00) >> 8).toByte, ((value & 0x00FF0000) >> 16).toByte, ((value & 0xFF000000) >> 24).toByte)
  def size = 4
  def asInt = value.toInt
  def asLong = value.toLong
  def negate = this.copy(value = -this.value)
}

case class Constant64(value: Long) extends Constant[_64] {
  self =>
  def getBytes: Array[Byte] = {
    import java.nio.{ByteBuffer, ByteOrder}
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

trait RegisterOrMemory[Size <: OperandSize]



