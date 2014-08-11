package com.scalaAsm.x86
package Instructions

import Standard._
import com.scalaAsm.x86.Operands.TwoOperandFormat
import com.scalaAsm.x86.Operands.OneOperandFormat

trait Sized[X] {
  val size: Int
}

trait Catalog extends Formats {

  def callNear[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: CALL_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size))

  def add[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: ADD_2[OpEn, O1, O2], size: InstructionSize[O1,O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, size,format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size))

  def or[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: OR_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size))
  
  def sub[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SUB_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size))

  def mul[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: MUL_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size))

  def push[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: PUSH_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size))

  def pop[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: POP_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size))

  def dec[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: DEC_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size))

  def and[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: AND_2[OpEn, O1, O2], size: InstructionSize[O1,O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1,p2, size, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size))

  def not[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: NOT_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size))

  def lea[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: LEA_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size))

  def jmp[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: JMP_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size))
  
  def mov[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: MOV_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size))

  def shr[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SHR_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size))

  def jnz[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: JNZ_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size))

  def jz[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: JZ_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size))

  def int[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: INT_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size))

  def shl[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SHL_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size))

  def sbb[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SBB_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size))

  def retn[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: RETN_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size))

  def retn(implicit ev: RET) = ev.get
  
  def leave(implicit ev: LEAVE) =  ev.get

  def test[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: TEST_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size))

  def rdrand[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: RDRAND_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size))
}