package com.scalaAsm.asm

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress

trait Addressing {
  def *[A <: GPR, B: Numeric](mem: A#BaseIndex[B]): Operand[A#BaseIndex[B]] = new Operand[A#BaseIndex[B]]{def apply = mem}

  def *[C: Numeric](offset: Constant[C])(implicit abs: AbsoluteAddress[C]): AbsoluteAddress[C] = { abs.offset = offset.value; abs }

  def *[Y <: GeneralPurpose[_]](gpr: Operand[Y]): Operand[Y#Indirect] = new Operand[Y#Indirect]{def apply = gpr().getIndirect}
  
}