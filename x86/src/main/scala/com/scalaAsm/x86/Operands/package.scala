package com.scalaAsm.x86

package object Operands {

  type GPR = GeneralPurpose[_ <: OperandSize]
  type SegmentRegister = GeneralPurpose[_16]

}