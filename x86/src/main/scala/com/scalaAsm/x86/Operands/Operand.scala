package com.scalaAsm.x86
package Operands

trait Operand

trait DualOperand[-O1,-O2] extends Operand {
  protected[this] var op1: O1 = _
  protected[this] var op2: O2 = _
  def set(x:O1, y:O2) = {
    op1 = x
    op2 = y
  }
}

trait SingleOperand[-O1] extends Operand {
  protected[this] var op1: O1 = _
  def set(x:O1) = {
    op1 = x
  }
}

sealed class OperandSize {
  type size
}

class ByteOperand extends OperandSize { type size = Byte }
class WordOperand extends OperandSize { type size = Short }
class DwordOperand extends OperandSize { type size = Int }
class QwordOperand extends OperandSize { type size = Long }

trait Displacement extends Any with InstructionField {
  type Size <: OperandSize
  type Offset <: Displacement
  def value: Size#size
  def negate: Offset
  def getBytes: Array[Byte]
  def size: Int
  override def toString = value.toString
  def isNegative: Boolean
}

case class Displacement8(value: Byte) extends AnyVal with Displacement {
  type Size = ByteOperand
  type Offset = Displacement8
  def negate: Displacement8 = Displacement8((-value).toByte)
  def getBytes: Array[Byte] = Array(value)
  def isNegative: Boolean = value < 0
  def size: Int = 1
}

case class Displacement16(value: Short) extends AnyVal with Displacement {
  type Size = WordOperand
  type Offset = Displacement16
  def negate: Displacement16 = Displacement16((-value).toShort)
  def getBytes: Array[Byte] = Array((value & 0x00FF).toByte, ((value & 0xFF00) >> 8).toByte)
  def isNegative: Boolean = value < 0
  def size: Int = 2
}

case class Displacement32(val value: Int) extends AnyVal with Displacement {
  type Size = DwordOperand
  type Offset = Displacement32
  def negate: Displacement32 = Displacement32(-value)
  def getBytes: Array[Byte] = Array((value & 0x000000FF).toByte, ((value & 0x0000FF00) >> 8).toByte, ((value & 0x00FF0000) >> 16).toByte, ((value & 0xFF000000) >> 24).toByte)
  def isNegative: Boolean = value < 0
  def size: Int = 4
}

trait Memory extends RegisterOrMemory {
  def base: Option[GPR]
  def offset: Option[Displacement]
  def immediate: Option[Immediate]
  
  def rel32: Relative32 = new Relative32 {
    def offset = Some(Displacement32(immediate.get.asInt))
  }
  
  override def toString = {
    var result: String = ""
    
    result = "[" + base.toString
    if (offset.isDefined) {
      if (!offset.get.isNegative)
    	  result += " + " + offset.get.toString
      else
    	  result += " - " + offset.get.negate.toString
    }
    result += "]"
    
    result
  }
}

trait Relative extends RegisterOrMemory {
    def offset: Option[Displacement]
}

trait Relative32 extends Relative {
  type Size = DwordOperand
}

trait RegisterOrMemory extends Any {
  type Size <: OperandSize
}



