package com.scalaAsm.x86

import com.scalaAsm.x86.Operands.GeneralPurpose
import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.Operands.Constant
import com.scalaAsm.x86.Operands._

package object Instructions {

  trait NP

  trait M
  trait O
  trait I
  trait Offset

  trait MR
  trait OI
  trait RM
  trait M1
  trait MI
  
  type rel = Relative[_ <: OperandSize]
  type rel16 = Relative[WordOperand]
  type rel32 = Relative[DwordOperand]
  type rel64 = Relative[QwordOperand]
  
  trait testOneOperand[X[G,H] <: OneOperandInstruction[G,H]] {
    def apply[O1: Sized, OpEn](p1: O1)(implicit ev: X[OpEn,O1], format: OneOperandFormat[OpEn, O1], prefix: HasRexPrefix[O1]) = ev(p1, format, implicitly[HasRexPrefix[O1]].get)
  }
  
  trait testTwoOperands[X[G,H,I] <: TwoOperandInstruction[G,H,I]] {
    def apply[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: X[OpEn,O1,O2], format: TwoOperandFormat[OpEn, O1, O2], prefix: HasRexPrefix[O1]) = ev(p1,p2, format, implicitly[HasRexPrefix[O1]].get)
  }
  
  class testZeroOperands[X <: ZeroOperandInstruction] {
    def apply(ignored: Unit)(implicit ev: X) = ev.get
  }
}