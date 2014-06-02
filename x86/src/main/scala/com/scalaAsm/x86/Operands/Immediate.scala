package com.scalaAsm.x86
package Operands

import java.nio.ByteBuffer
import java.nio.ByteOrder

trait Immediate extends InstructionField with ConstantOperand with Operand {
  def size: Int
  override def toString = value.toString
}

trait Immediate8 extends Immediate with ConstantOperand8

trait Immediate16 extends Immediate with ConstantOperand16

trait Immediate32 extends Immediate with ConstantOperand32

trait Immediate64 extends Immediate with ConstantOperand64