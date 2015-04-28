package com.scalaAsm.asm

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress

trait Addressing {
  def *[C: Numeric](offset: Constant[C])(implicit abs: AbsoluteAddress[C]): AbsoluteAddress[C] = { abs.offset = offset.value; abs }

  def *[Y: x86Size](gpr: GeneralPurpose[Y]) = new gpr.Indirect {}
  
}