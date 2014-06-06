package com.scalaAsm.x86
package Operands

import java.nio.ByteBuffer
import java.nio.ByteOrder
import com.scalaAsm.x86.Operands.Memory._
import com.scalaAsm.x86.Operands.Memory.WithSIB
import com.scalaAsm.x86.Operands.Memory.SIB
import com.scalaAsm.x86.Operands.Memory.NoSIB
import com.scalaAsm.x86.Operands.Memory.NoModRM
import com.scalaAsm.x86.Operands.Memory.NoDisplacement
import com.scalaAsm.x86.Operands.Memory.ModRMReg
import com.scalaAsm.x86.Operands.Memory.ModRMOpcode
import com.scalaAsm.x86.Operands.Memory.DisplacementByte
import com.scalaAsm.x86.Operands.Memory.AddressingFormSpecifier

trait Operand {
  type Size <: OperandSize
}

trait Constant extends InstructionField {
  type Size <: OperandSize
  def value: Size#size
  def getBytes: Array[Byte]
  def asInt: Int
  def asLong: Long
}

trait Constant8 extends Constant {
  type Size = ByteOperand
  def getBytes: Array[Byte] = Array(value)
  def size = 1
  def asInt = value.toInt
  def asLong = value.toLong
}

trait Constant16 extends Constant {
  type Size = WordOperand
  def getBytes: Array[Byte] = Array((value & 0x00FF).toByte, ((value & 0xFF00) >> 8).toByte)
  def size = 2
  def asInt = value.toInt
  def asLong = value.toLong
}

trait Constant32 extends Constant {
  type Size = DwordOperand
  def getBytes: Array[Byte] = Array((value & 0x000000FF).toByte, ((value & 0x0000FF00) >> 8).toByte, ((value & 0x00FF0000) >> 16).toByte, ((value & 0xFF000000) >> 24).toByte)
  def size = 4
  def asInt = value.toInt
  def asLong = value.toLong
}

trait Constant64 extends Constant {
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
}

sealed trait OperandSize {
  type size
}

class ByteOperand extends OperandSize { type size = Byte }
class WordOperand extends OperandSize { type size = Short }
class DwordOperand extends OperandSize { type size = Int }
class QwordOperand extends OperandSize { type size = Long }

trait AddressingMode extends RegisterOrMemory

trait ImmediateMemory extends AddressingMode {
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

trait RegisterIndirect extends AddressingMode {
  self =>
  def base: GPR

  def encode(reg: GPR, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    NoSIB(ModRMReg(NoDisplacement, reg, rm = base))
  }
  
  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    NoSIB(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
  }
}


trait BaseIndex extends AddressingMode {
  self =>
  def base: GPR
  def offset: Displacement

  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    (base, offset) match {
      case (base: Register64, _) =>
        WithSIB(ModRMOpcode(NoDisplacement, opcodeExtend.get, base), SIB(SIB.One, new ESP, base))
      case (base, _: Displacement8) =>
        NoSIB(ModRMOpcode(DisplacementByte, opcodeExtend.get, base))
      //case (base, None) =>
       // NoSIB(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
      case _ => NoModRM()
    }
  }
  
//  override def toString = {
//    var result: String = ""
//    
//    result = "[" + base.toString
//    if (offset.isDefined) {
//      if (!offset.get.isNegative)
//    	  result += " + " + offset.get.toString
//      else
//    	  result += " - " + offset.get.negate.toString
//    }
//    result += "]"
//    
//    result
//  }
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



