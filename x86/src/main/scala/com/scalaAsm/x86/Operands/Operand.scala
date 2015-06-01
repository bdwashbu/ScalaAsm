package com.scalaAsm.x86
package Operands

import com.scalaAsm.x86.Instructions.InstructionField
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import java.nio.{ByteBuffer, ByteOrder}

case class Constant[Size: x86Size](val value: Size)(implicit writer: ConstantWriter[Size]) extends InstructionField {
  
  val buffer = ByteBuffer.allocate(size)
  if (size != 0) {
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    writer.write(buffer, value)
  }
  
  def negate: Constant[Size] = new Constant[Size](getNegative) {}
  def size = implicitly[x86Size[Size]].size
  protected def getNegative = implicitly[x86Size[Size]].negate(value)
  def getBytes: Array[Byte] = {
      buffer.array()
  }

  override def toString = value.toString
}

object ConstantWriter {
  implicit object Constant8 extends ConstantWriter[_8] {
    def write(buffer: ByteBuffer, value: _8) = buffer.put(value)
  }
  
  implicit object Constant16 extends ConstantWriter[_16] {
    def write(buffer: ByteBuffer, value: _16) = buffer.putShort(value)
  }
  
  implicit object Constant32 extends ConstantWriter[_32] {
    def write(buffer: ByteBuffer, value: _32) = buffer.putInt(value)
  }
  
  implicit object Constant64 extends ConstantWriter[_64] {
    def write(buffer: ByteBuffer, value: _64) = buffer.putLong(value)
  }
}

abstract class ConstantWriter[Size: x86Size] {
  def write(buffer: ByteBuffer, value: Size): Unit
}

class RegisterOrMemory[Size: x86Size] {
  def size = implicitly[x86Size[Size]].size
}