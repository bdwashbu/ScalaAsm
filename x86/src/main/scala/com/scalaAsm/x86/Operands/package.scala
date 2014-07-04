package com.scalaAsm.x86

package object Operands {

  type GPR = Register with GeneralPurpose
  type SegmentRegister = Register16
  
  type TwoOperands[X <: Operand, Y <: Operand] = Tuple2[X, Y]
  type OneOperand[X <: Operand] = Tuple1[X]
}