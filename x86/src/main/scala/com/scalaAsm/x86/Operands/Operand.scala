package com.scalaAsm.x86
package Operands

import com.scalaAsm.x86.Instructions.InstructionField
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import java.nio.{ByteBuffer, ByteOrder}

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