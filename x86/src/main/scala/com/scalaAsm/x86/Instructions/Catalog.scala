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
  
//  implicit object size1 extends Sized[CS] {val size = 2}
//  implicit object size2 extends Sized[DS] {val size = 2}
//  implicit object size3 extends Sized[DX] {val size = 2}
//  implicit object size4 extends Sized[DH] {val size = 2}
//  implicit object size5 extends Sized[AX] {val size = 2}
//  implicit object size6 extends Sized[AH] {val size = 2}
  

  def callNear[O1: Sized, OpEn](p1: O1)(implicit ev: CALL_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def add[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: ADD_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def or[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: OR_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))
  
  def sub[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SUB_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def mul[O1: Sized, OpEn](p1: O1)(implicit ev: MUL_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def push[O1: Sized, OpEn](p1: O1)(implicit ev: PUSH_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def pop[O1: Sized, OpEn](p1: O1)(implicit ev: POP_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def dec[O1: Sized, OpEn](p1: O1)(implicit ev: DEC_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def and[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: AND_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1,p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def not[O1: Sized, OpEn](p1: O1)(implicit ev: NOT_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def lea[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: LEA_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def jmp[O1: Sized, OpEn](p1: O1)(implicit ev: JMP_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))
  
  def mov[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: Mov2Def[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev(p1, p2, format)

  def shr[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SHR_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def jnz[O1: Sized, OpEn](p1: O1)(implicit ev: JNZ_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def jz[O1: Sized, OpEn](p1: O1)(implicit ev: JZ_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def int[O1: Sized, OpEn](p1: O1)(implicit ev: INT_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def shl[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SHL_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def sbb[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: SBB_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def retn[O1: Sized, OpEn](p1: O1)(implicit ev: RETN_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))

  def retn(implicit ev: RET) = ev.get
  
  def leave(implicit ev: LEAVE) =  ev.get

  def test[O1: Sized, O2: Sized, OpEn](p1: O1, p2: O2)(implicit ev: TEST_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format(implicitly[Sized[O1]].size, implicitly[Sized[O2]].size, ev.opcode))

  def rdrand[O1: Sized, OpEn](p1: O1)(implicit ev: RDRAND_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format(implicitly[Sized[O1]].size, ev.opcode))
}