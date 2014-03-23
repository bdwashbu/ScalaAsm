package com.scalaAsm.x86

import com.scalaAsm.x86.Operands.x86Registers._

package object Operands {

  type GeneralPurposeRegister = Register with GeneralPurpose
  type GPR = GeneralPurposeRegister
}