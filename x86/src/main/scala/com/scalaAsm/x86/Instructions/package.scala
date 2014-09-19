package com.scalaAsm.x86

import com.scalaAsm.x86.Operands.GeneralPurpose
import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.Operands.Constant
import com.scalaAsm.x86.Operands._

package object Instructions {

  trait TwoOperandEncoding[-O1, -O2]
  trait OneOperandEncoding[-O1]
  
  trait NP

  trait M extends OneOperandEncoding[rm]
  trait O extends OneOperandEncoding[rm] // can we define this better?
  trait I extends OneOperandEncoding[imm]
  trait Offset extends OneOperandEncoding[rm] // is this different than O?

  trait MR extends TwoOperandEncoding[rm, r]
  trait OI extends TwoOperandEncoding[rm, imm] // can we define this better
  trait RM extends TwoOperandEncoding[r, rm]
  trait M1 extends TwoOperandEncoding[rm, One]
  trait MI extends TwoOperandEncoding[rm, imm]
  
  trait CSFormat extends OneOperandEncoding[CS]
  trait DSFormat extends OneOperandEncoding[DS]
  
  type rel = Relative[_ <: OperandSize]
  type rel16 = Relative[_16]
  type rel32 = Relative[_32]
  type rel64 = Relative[_64]
 

  

  //trait testOneOperand[X <: x86Instruction] {
  //  def apply[O1: Sized, OpEn <: OneOperandEncoding[O1]](p1: Operand[_,O1])(implicit ev: OneOperandInstruction[O1, OpEn] with X, format: OneOperandFormat[O1, OpEn], prefix: HasRexPrefix[O1]) = ev(p1, format, implicitly[HasRexPrefix[O1]].get)
  //}
  
  trait testOneOperand[X[G,OpEn <: OneOperandEncoding[G]] <: OneOperandInstruction[G,OpEn,_]] {
    def apply[O1: Sized, OpEn <: OneOperandEncoding[O1], Opcode](p1: Operand[_,O1])(implicit ev: X[O1, OpEn], format: OneOperandFormat[O1, OpEn], prefix: HasRexPrefix[O1]) = ev(p1, format, implicitly[HasRexPrefix[O1]].get)
  }
  
  //trait testTwoOperands[X <: x86Instruction] {
  //  def apply[O1: Sized, O2: Sized, OpEn <: TwoOperandEncoding[O1,O2]](p1: Operand[_,O1], p2: Operand[_,O2])(implicit ev: TwoOperandInstruction[O1, O2, OpEn] with X, format: TwoOperandFormat[O1, O2, OpEn], prefix: HasRexPrefix[O1]) = ev(p1,p2, format, implicitly[HasRexPrefix[O1]].get)
  //}
  
  trait testTwoOperands[X[G,H,OpEn <: TwoOperandEncoding[G,H]] <: TwoOperandInstruction[G,H,OpEn,_]] {
    def apply[O1: Sized, O2: Sized, OpEn <: TwoOperandEncoding[O1,O2], Opcode](p1: Operand[_,O1], p2: Operand[_,O2])(implicit ev: X[O1, O2, OpEn], format: TwoOperandFormat[O1, O2, OpEn], prefix: HasRexPrefix[O1]) = ev(p1,p2, format, implicitly[HasRexPrefix[O1]].get)
  }
  
  class testZeroOperands[X <: ZeroOperandInstruction[_]] {
    def apply(ignored: Unit)(implicit ev: X) = ev.get
  }
}