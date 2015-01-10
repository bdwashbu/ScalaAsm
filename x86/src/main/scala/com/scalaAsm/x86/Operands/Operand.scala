package com.scalaAsm.x86
package Operands

import com.scalaAsm.x86.Instructions.InstructionField
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress

trait One extends Operand[One]{
  def size = 1
  def get = this
}

case class addr(name: String) extends Operand[AbsoluteAddress[_32]] {
  def get = new AbsoluteAddress[_32] {
    var offset = 0
    def getRelative = null
    def displacement = Constant32(0)
  }
}

trait Operand[+To] {
  def get: To
}

case class Op[X](from: X) extends Operand[X] {
  def get = from
  override def toString = from.toString
}

abstract class Constant[Size: x86Size](val value: Size) extends InstructionField {
  import java.nio.{ByteBuffer, ByteOrder}
  val buffer = ByteBuffer.allocate(size)
  buffer.order(ByteOrder.LITTLE_ENDIAN)
  def negate: Constant[Size]
  def size = implicitly[x86Size[Size]].size
  protected def getNegative = implicitly[x86Size[Size]].negate(value)
  def getBytes: Array[Byte] = {
      buffer.array()
  }
  
  override def toString = value.toString
}

case class Constant8(x: Byte) extends Constant[_8](x) {
  self =>
  buffer.put(value)
  def negate = Constant8(getNegative)
}

case class Constant16(x: Short) extends Constant[_16](x) {
  self =>
  buffer.putShort(value)
  def negate = Constant16(getNegative)
}

case class Constant32(x: Int) extends Constant[_32](x) {
  self =>
  buffer.putInt(value)
  def negate = Constant32(getNegative)
}

case class Constant64(x: Long) extends Constant[_64](x) {
  self =>
  buffer.putLong(value)
  def negate = Constant64(getNegative)
}

class RegisterOrMemory[Size: x86Size]