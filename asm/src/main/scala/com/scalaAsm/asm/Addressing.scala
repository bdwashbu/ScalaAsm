package com.scalaAsm.asm

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

trait Addressing {
  def *[C: x86Size](offset: Constant[C]) = AbsoluteAddress[C](offset.value)

  def *[Y: x86Size](gpr: GeneralPurpose[Y]) = new gpr.Indirect {}
  
}