package com.scalaAsm.x86
package Operands

import java.nio.ByteBuffer
import java.nio.ByteOrder

trait Operand extends Any

sealed trait OperandSize {
  type size
  val length: Int
}

class ByteOperand extends OperandSize { type size = Byte; val length = 1 }
class WordOperand extends OperandSize { type size = Short; val length = 2 }
class DwordOperand extends OperandSize { type size = Int; val length = 4 }
class QwordOperand extends OperandSize { type size = Long; val length = 8 }

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

case class Displacement64(val value: Long) extends AnyVal with Displacement {
  type Size = QwordOperand
  type Offset = Displacement64
  def negate: Displacement64 = Displacement64(-value)
  def getBytes: Array[Byte] = {
     val buffer = ByteBuffer.allocate(8)
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      buffer.putLong(value)
      buffer.array()
  }
  def isNegative: Boolean = value < 0
  def size: Int = 8
}

trait Memory extends RegisterOrMemory {
  def base: Option[GPR]
  def offset: Option[Displacement]
  def immediate: Option[Immediate]
  
  def rel32: Relative32 = new Relative32 {
    def offset = Some(Displacement32(immediate.get.asInt))
  }
  
  def rel64: Relative64 = new Relative64 {
    def offset = Some(Displacement64(immediate.get.asLong))
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

trait Relative64 extends Relative {
  type Size = QwordOperand
}

trait RegisterOrMemory extends Any with Operand {
  type Size <: OperandSize
}



