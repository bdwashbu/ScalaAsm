package com.scalaAsm.x86

package object Operands {

  type GeneralPurposeRegister = Register with GeneralPurpose
  type GPR = GeneralPurposeRegister
  type SegmentRegister = Register16
}