package com.scalaAsm.x86
package Operands

import java.nio.ByteBuffer
import java.nio.ByteOrder

trait Immediate extends InstructionField with Constant with Operand {
  override def toString = value.toString
}

trait Immediate8 extends Immediate with Constant8

trait Immediate16 extends Immediate with Constant16

trait Immediate32 extends Immediate with Constant32

trait Immediate64 extends Immediate with Constant64