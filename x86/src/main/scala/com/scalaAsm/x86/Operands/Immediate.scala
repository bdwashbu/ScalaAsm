package com.scalaAsm.x86
package Operands

import java.nio.ByteBuffer
import java.nio.ByteOrder

trait Immediate extends InstructionField with ConstantOperand with Operand {
  def asInt: Int
  def asLong: Long
  def size: Int
  override def toString = value.toString
}

trait Immediate8 extends Immediate with ConstantOperand8 {
  def asInt = value.toInt
  def asLong = value.toLong
}

trait Immediate16 extends Immediate with ConstantOperand16 {
  def asInt = value.toInt
  def asLong = value.toLong
}

trait Immediate32 extends Immediate with ConstantOperand32 {
  def asInt = value.toInt
  def asLong = value.toLong
}

trait Immediate64 extends Immediate with ConstantOperand64 {
  def asInt = value.toInt
  def asLong = value.toLong
}