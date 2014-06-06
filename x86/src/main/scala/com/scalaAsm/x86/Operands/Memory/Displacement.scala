package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.InstructionField

trait Displacement extends InstructionField with Constant {
  def negate: Displacement
  override def toString = value.toString
  def isNegative: Boolean
}

trait Displacement8 extends Displacement with Constant8 {
  self =>
  def negate: Displacement8 = new Displacement8 {
	  override def negate: Displacement8 = self
	  val value = (-self.value).toByte
  }
  def isNegative: Boolean = value < 0
}

trait Displacement16 extends Displacement with Constant16 {
  self =>
  def negate: Displacement16 = new Displacement16 {
	  override def negate: Displacement16 = self
	  val value: Short = (-self.value).toShort
  }
  def isNegative: Boolean = value < 0
}

trait Displacement32 extends Displacement with Constant32 {
  self =>
  def negate: Displacement32 = new Displacement32{
	  override def negate: Displacement32 = self
	  val value: Int = -self.value
  }
  def isNegative: Boolean = value < 0
}

trait Displacement64 extends Displacement with Constant64 {
  self =>
  def negate: Displacement64 = new Displacement64{
	  override def negate: Displacement64 = self
	  val value: Long = -self.value
  }
  def isNegative: Boolean = value < 0
}