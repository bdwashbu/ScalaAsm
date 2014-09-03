package com.scalaAsm.x86

import com.scalaAsm.x86.Operands.GeneralPurpose
import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.Operands.Constant
import com.scalaAsm.x86.Operands._

package object Instructions {

  trait NP

  trait M[rm]
  trait O[rm] // can we define this better?
  trait I[imm]
  trait Offset[rm] // is this different than O?

  trait MR[rm, r]
  trait OI[rm, imm] // can we define this better
  trait RM[r, rm]
  trait M1[rm, One]
  trait MI[rm, imm]
  
  trait CSFormat[CS]
  trait DSFormat[DS]
  
  type rel = Relative[_ <: OperandSize]
  type rel16 = Relative[WordOperand]
  type rel32 = Relative[DwordOperand]
  type rel64 = Relative[QwordOperand]
 

  case class Op[X](from: X) extends Operand[X, X] { def get = from }
  
  trait testOneOperand[X[G,H[_]] <: OneOperandInstruction[G,H]] {
    def apply[O1: Sized, OpEn[O1]](p1: Operand[_,O1])(implicit ev: X[O1, OpEn], format: OneOperandFormat[O1, OpEn], prefix: HasRexPrefix[O1]) = ev(p1, format, implicitly[HasRexPrefix[O1]].get)
  }
  
  trait testTwoOperands[X[G,H,I[_,_]] <: TwoOperandInstruction[G,H,I]] {
    def apply[O1: Sized, O2: Sized, OpEn[O1,O2]](p1: Operand[_,O1], p2: Operand[_,O2])(implicit ev: X[O1, O2, OpEn], format: TwoOperandFormat[O1, O2, OpEn], prefix: HasRexPrefix[O1]) = ev(p1,p2, format, implicitly[HasRexPrefix[O1]].get)
  }
  
  class testZeroOperands[X <: ZeroOperandInstruction] {
    def apply(ignored: Unit)(implicit ev: X) = ev.get
  }
}