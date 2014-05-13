package com.scalaAsm.x86
package Operands

import java.nio.ByteBuffer
import java.nio.ByteOrder

trait Operand {
  type Size <: OperandSize
  def size: Int
}

trait ConstantOperand extends Operand {
  def value: Size#size
  def getBytes: Array[Byte]
}

trait ConstantOperand8 extends ConstantOperand {
  type Size = ByteOperand
  def getBytes: Array[Byte] = Array(value)
  def size: Int = 1
}

trait ConstantOperand16 extends ConstantOperand {
  type Size = WordOperand
  def getBytes: Array[Byte] = Array((value & 0x00FF).toByte, ((value & 0xFF00) >> 8).toByte)
  def size: Int = 2
}

trait ConstantOperand32 extends ConstantOperand {
  type Size = DwordOperand
 def getBytes: Array[Byte] = Array((value & 0x000000FF).toByte, ((value & 0x0000FF00) >> 8).toByte, ((value & 0x00FF0000) >> 16).toByte, ((value & 0xFF000000) >> 24).toByte)
  def size: Int = 4
}

trait ConstantOperand64 extends ConstantOperand {
  type Size = QwordOperand
  def getBytes: Array[Byte] = {
    val buffer = ByteBuffer.allocate(8)
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      buffer.putLong(value)
      buffer.array()
  }
  def size: Int = 8
}

sealed trait OperandSize {
  type size
  val length: Int
}

class ByteOperand extends OperandSize { type size = Byte; val length = 1 }
class WordOperand extends OperandSize { type size = Short; val length = 2 }
class DwordOperand extends OperandSize { type size = Int; val length = 4 }
class QwordOperand extends OperandSize { type size = Long; val length = 8 }

trait Displacement extends InstructionField with ConstantOperand {
  type Offset <: Displacement
  def negate: Offset
  override def toString = value.toString
  def isNegative: Boolean
}

trait Displacement8 extends Displacement with ConstantOperand8 {
  self =>
  type Offset = Displacement8
  def negate: Displacement8 = new Displacement8 {
	  override def negate: Displacement8 = self
	  val value = (-self.value).toByte
  }
  def isNegative: Boolean = value < 0
}

trait Displacement16 extends Displacement with ConstantOperand16 {
  self =>
  type Offset = Displacement16
  def negate: Displacement16 = new Displacement16 {
	  override def negate: Displacement16 = self
	  val value = (-self.value).toShort
  }
  def isNegative: Boolean = value < 0
}

trait Displacement32 extends Displacement with ConstantOperand32 {
  self =>
  type Offset = Displacement32
  def negate: Displacement32 = new Displacement32{
	  override def negate: Displacement32 = self
	  val value = (-self.value).toInt
  }
  def isNegative: Boolean = value < 0
}

trait Displacement64 extends Displacement with ConstantOperand64 {
  self =>
  type Offset = Displacement64
  def negate: Displacement64 = new Displacement64{
	  override def negate: Displacement64 = self
	  val value = (-self.value).toLong
  }
  def isNegative: Boolean = value < 0
}

trait Memory extends RegisterOrMemory {
  def base: Option[GPR]
  def offset: Option[Displacement]
  def immediate: Option[Immediate]
  
  def rel32: Relative32 = new Relative32 {
    def offset = Some(new Displacement32{ val value = immediate.get.asInt})
    def size = 4
  }
  
  def rel64: Relative64 = new Relative64 {
    def offset = Some(new Displacement64{ val value = immediate.get.asLong})
    def size = 8
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

trait RegisterOrMemory extends Operand {
  type Size <: OperandSize
}



