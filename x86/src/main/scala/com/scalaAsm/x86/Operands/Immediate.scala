package com.scalaAsm.x86
package Operands

trait Immediate extends Any with InstructionField {
  type Size <: OperandSize
  def value: Size#size
  def getBytes: Array[Byte]
  def asInt: Int
  def size: Int
  override def toString = value.toString
}

case class Immediate8(value: Byte) extends AnyVal with Immediate {
  type Size = ByteOperand
  def getBytes: Array[Byte] = Array(value)
  def size: Int = 1
  def asInt = value.toInt
}

case class Immediate16(value: Short) extends AnyVal with Immediate {
  type Size = WordOperand
  def getBytes: Array[Byte] = Array((value & 0x00FF).toByte, ((value & 0xFF00) >> 8).toByte)
  def size: Int = 2
  def asInt = value.toInt
}

case class Immediate32(val value: Int) extends AnyVal with Immediate {
  type Size = DwordOperand
  def getBytes: Array[Byte] = Array((value & 0x000000FF).toByte, ((value & 0x0000FF00) >> 8).toByte, ((value & 0x00FF0000) >> 16).toByte, ((value & 0xFF000000) >> 24).toByte)
  def size: Int = 4
  def asInt = value.toInt
}