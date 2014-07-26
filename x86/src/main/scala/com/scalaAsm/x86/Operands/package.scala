package com.scalaAsm.x86

package object Operands {

  type GPR[Self] = GeneralPurpose[Self]
  type SegmentRegister = Register16

}