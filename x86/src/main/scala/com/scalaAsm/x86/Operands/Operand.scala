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
  def asInt: Int
  def asLong: Long
}

trait ConstantOperand8 extends ConstantOperand {
  type Size = ByteOperand
  def getBytes: Array[Byte] = Array(value)
  def size: Int = 1
  def asInt = value.toInt
  def asLong = value.toLong
}

trait ConstantOperand16 extends ConstantOperand {
  type Size = WordOperand
  def getBytes: Array[Byte] = Array((value & 0x00FF).toByte, ((value & 0xFF00) >> 8).toByte)
  def size: Int = 2
  def asInt = value.toInt
  def asLong = value.toLong
}

trait ConstantOperand32 extends ConstantOperand {
  type Size = DwordOperand
 def getBytes: Array[Byte] = Array((value & 0x000000FF).toByte, ((value & 0x0000FF00) >> 8).toByte, ((value & 0x00FF0000) >> 16).toByte, ((value & 0xFF000000) >> 24).toByte)
  def size: Int = 4
  def asInt = value.toInt
  def asLong = value.toLong
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
  def asInt = value.toInt
  def asLong = value.toLong
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

trait ImmediateMemory extends RegisterOrMemory {
  self =>
  def immediate: Immediate { type Size = self.Size}
  
  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    NoSIB(ModRMOpcode(NoDisplacement, opcodeExtend.get, new EBP))
  }
  
  def rel32: Relative32 = new Relative32 {
    def offset = new Displacement32{ val value = self.immediate.asInt}
    def size = 4
  }
  
  def rel64: Relative64 = new Relative64 {
    def offset = new Displacement64{ val value = self.immediate.asLong}
    def size = 8
  }
}

trait RegisterIndirect extends RegisterOrMemory {
  self =>
  def base: GPR

  def encode(reg: GPR, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    //NoSIB(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
    NoSIB(ModRMReg(NoDisplacement, reg, rm = base))
  }
  
  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    NoSIB(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
  }
}

trait Memory extends RegisterOrMemory {
  self =>
  def base: Option[GPR]
  def offset: Option[Displacement]

  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    (base, offset) match {
      case (Some(base: Register64), _) =>
        WithSIB(ModRMOpcode(NoDisplacement, opcodeExtend.get, base), SIB(SIB.One, new ESP, base))
      case (Some(base), Some(_: Displacement8)) =>
        NoSIB(ModRMOpcode(DisplacementByte, opcodeExtend.get, base))
      case (Some(base), None) =>
        NoSIB(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
      case _ => NoModRM()
    }
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
  self =>
    def offset: Displacement {type Size = self.Size}
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



