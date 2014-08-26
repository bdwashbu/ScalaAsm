package com.scalaAsm.x86
package Instructions

import Standard._
import com.scalaAsm.x86.Operands.TwoOperandFormat
import com.scalaAsm.x86.Operands.OneOperandFormat
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.Operands.Memory.RegisterIndirect

trait Catalog extends Formats {

  def callNear[O1: Sized, OpEn](p1: O1)(implicit ev: CALL_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev(p1, format)

  def add[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: ADD_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev(p1, p2, format)

  def or[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: OR_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev(p1, p2, format)
  
  def sub[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SUB_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev(p1, p2, format)

  def mul[O1: Sized, OpEn](p1: O1)(implicit ev: MUL_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev(p1, format)

  def push[O1: Sized, OpEn](p1: O1)(implicit ev: PUSH_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev(p1, format)

  def pop[O1: Sized, OpEn](p1: O1)(implicit ev: POP_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev(p1, format)

  def dec[O1: Sized, OpEn](p1: O1)(implicit ev: DEC_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev(p1, format)

  def and[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: AND_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev(p1,p2, format)

  def not[O1: Sized, OpEn](p1: O1)(implicit ev: NOT_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev(p1, format)

  def lea[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: LEA_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev(p1, p2, format)

  def jmp[O1: Sized, OpEn](p1: O1)(implicit ev: JMP_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev(p1, format)
  
  def mov[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: Mov2Def[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev(p1, p2, format)

  def shr[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SHR_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev(p1, p2, format)

  def jnz[O1: Sized, OpEn](p1: O1)(implicit ev: JNZ_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev(p1, format)

  def jz[O1: Sized, OpEn](p1: O1)(implicit ev: JZ_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev(p1, format)

  def int[O1: Sized, OpEn](p1: O1)(implicit ev: INT_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev(p1, format)

  def shl[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SHL_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev(p1, p2, format)

  def sbb[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SBB_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev(p1, p2, format)

  def retn[O1: Sized, OpEn](p1: O1)(implicit ev: RETN_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev(p1, format)

  def retn(implicit ev: RET) = ev.get
  
  def leave(implicit ev: LEAVE) =  ev.get

  def test[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: TEST_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev(p1, p2, format)

  def rdrand[O1: Sized, OpEn](p1: O1)(implicit ev: RDRAND_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev(p1, format)
}