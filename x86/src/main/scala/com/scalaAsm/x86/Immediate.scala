package com.scalaAsm.x86

trait Immediate extends Any  {
  type immType
  type X <: Immediate
  implicit def numericT: Numeric[immType]
  def zero = numericT.zero
  def value: immType
  def negate: X
  def getBytes: Array[Byte]
  def size: Int
  override def toString = value.toString
  def isNegative: Boolean
}

case class Immediate8(value: Byte) extends AnyVal with Immediate {
  type immType = Byte
  implicit def numericT : Numeric[Byte] = implicitly[Numeric[Byte]]
  type X = Immediate8
  def negate: Immediate8 = Immediate8((-value).toByte)
  def getBytes: Array[Byte] = Array(value)
  def isNegative: Boolean = value < 0
  def size: Int = 1
}

case class Immediate16(value: Short) extends AnyVal with Immediate {
  type immType = Short
  implicit def numericT : Numeric[Short] = implicitly[Numeric[Short]]
  type X = Immediate16
  def negate: Immediate16 = Immediate16((-value).toShort)
  def getBytes: Array[Byte] = Array((value & 0x00FF).toByte, ((value & 0xFF00) >> 8).toByte)
  def isNegative: Boolean = value < 0
  def size: Int = 2
}

case class Immediate32(val value: Int) extends AnyVal with Immediate {
  type immType = Int
  implicit def numericT : Numeric[Int] = implicitly[Numeric[Int]]
  type X = Immediate32
  def negate: Immediate32 = Immediate32(-value)
  def getBytes: Array[Byte] = Array((value & 0x000000FF).toByte, ((value & 0x0000FF00) >> 8).toByte, ((value & 0x00FF0000) >> 16).toByte, ((value & 0xFF000000) >> 24).toByte)
  def isNegative: Boolean = value < 0
  def size: Int = 4
}