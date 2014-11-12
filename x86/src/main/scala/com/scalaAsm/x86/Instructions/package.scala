package com.scalaAsm.x86

import com.scalaAsm.x86.Operands.GeneralPurpose
import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.Operands.Constant
import com.scalaAsm.x86.Operands._
import scala.language.higherKinds
import scala.language.existentials

package object Instructions {

  trait TwoOperandEncoding[-O1, -O2]
  trait OneOperandEncoding[-O1]
  
  trait NP

  trait M extends OneOperandEncoding[rm]
  trait O extends OneOperandEncoding[rm] // can we define this better?
  trait I extends OneOperandEncoding[imm]
  trait Offset extends OneOperandEncoding[rm] // is this different than O?

  trait I2 extends TwoOperandEncoding[r, imm]
  trait MR extends TwoOperandEncoding[rm, r]
  trait OI extends TwoOperandEncoding[rm, imm] // can we define this better
  trait RM extends TwoOperandEncoding[r, rm]
  trait M1 extends TwoOperandEncoding[rm, One]
  trait MI extends TwoOperandEncoding[rm, imm]
  
  trait CSFormat extends OneOperandEncoding[CS]
  trait DSFormat extends OneOperandEncoding[DS]
  
  trait OneOperand[X[G,OpEn <: OneOperandEncoding[G]] <: OneOperandInstruction[G,OpEn,_]] {
    def apply[O1, OpEn <: OneOperandEncoding[O1], Opcode](p1: Operand[O1])(implicit ev: X[O1, OpEn], format: OneOperandFormat[O1, OpEn]) = ev(p1, format, ev.prefix)
  }
  
  trait TwoOperands[X[G,H,OpEn <: TwoOperandEncoding[G,H]] <: TwoOperandInstruction[G,H,OpEn,_]] {
    def apply[O1, O2, OpEn <: TwoOperandEncoding[O1,O2], Opcode](p1: Operand[O1], p2: Operand[O2])(implicit ev: X[O1, O2, OpEn], format: TwoOperandFormat[O1, O2, OpEn]) = ev(p1,p2, format, ev.prefix)
  }
  
  class ZeroOperands[X <: ZeroOperandInstruction[_]] {
    def apply(ignored: Unit)(implicit ev: X) = ev.get
  }
}