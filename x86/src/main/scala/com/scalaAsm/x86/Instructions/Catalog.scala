package com.scalaAsm.x86
package Instructions

import Standard._
import com.scalaAsm.x86.Operands.TwoOperandFormat
import com.scalaAsm.x86.Operands.OneOperandFormat
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.Operands.Memory.RegisterIndirect

trait Sized[-X] {
  val size: Int
}

trait Catalog extends Formats {
  
//  implicit object size1 extends Sized[CS] {val size = 2}
//  implicit object size2 extends Sized[DS] {val size = 2}
//  implicit object size3 extends Sized[DX] {val size = 2}
//  implicit object size4 extends Sized[DH] {val size = 2}
//  implicit object size5 extends Sized[AX] {val size = 2}
//  implicit object size6 extends Sized[AH] {val size = 2}
  implicit object size7 extends Sized[Relative{type Size = DwordOperand}] {val size = 4}
  implicit object size8 extends Sized[AbsoluteAddress[Constant32]] {val size = 4}
  implicit object size9 extends Sized[Register32] {val size = 4}
  implicit object size10 extends Sized[Constant8] {val size = 1}
  implicit object size11 extends Sized[BaseIndex[EBP, Constant8]] {val size = 1}
  implicit object size12 extends Sized[Register64] {val size = 8}
  implicit object size13 extends Sized[RegisterIndirect[RSP]] {val size = 8}
  implicit object size14 extends Sized[Register8] {val size = 1}
  implicit object size15 extends Sized[SegmentRegister] {val size = 2}
  implicit object size16 extends Sized[BaseIndex[RSP, Constant8]] {val size = 1}
  implicit object size17 extends Sized[BaseIndex[ESP, Constant8]] {val size = 1}
  implicit object size18 extends Sized[BaseIndex[EAX, Constant8]] {val size = 1}
  implicit object size19 extends Sized[BaseIndex[EDI, Constant32]] {val size = 4}
  implicit object size20 extends Sized[One] {val size = 1}

  def callNear[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: CALL_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def add[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: ADD_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def or[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: OR_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))
  
  def sub[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SUB_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def mul[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: MUL_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def push[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: PUSH_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def pop[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: POP_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def dec[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: DEC_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def and[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: AND_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1,p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def not[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: NOT_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def lea[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: LEA_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def jmp[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: JMP_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))
  
  def mov[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: MOV_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def shr[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SHR_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def jnz[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: JNZ_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def jz[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: JZ_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def int[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: INT_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def shl[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SHL_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def sbb[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SBB_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def retn[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: RETN_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def retn(implicit ev: RET) = ev.get
  
  def leave(implicit ev: LEAVE) =  ev.get

  def test[O1 <: Operand: Sized, O2 <: Operand: Sized, OpEn](p1: O1, p2: O2)(implicit ev: TEST_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def rdrand[O1 <: Operand: Sized, OpEn](p1: O1)(implicit ev: RDRAND_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))
}