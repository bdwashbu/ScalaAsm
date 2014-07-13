package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.InstructionSize

trait Catalog extends Formats {

  def callNear[O1 <: Operand, OpEn](p1: O1)(implicit ev: CALL_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format)

  def add[O1 <: Operand, O2 <: Operand, OpEn](p1: O1, p2: O2)(implicit ev: ADD_2[OpEn, O1, O2], size: InstructionSize[O1,O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, size,format)

  def or[O1 <: Operand, O2 <: Operand, OpEn](p1: O1, p2: O2)(implicit ev: OR_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format)
  
  def sub[O1 <: Operand, O2 <: Operand, OpEn](p1: O1, p2: O2)(implicit ev: SUB_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format)

  def mul[O1 <: Operand, OpEn](p1: O1)(implicit ev: MUL_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format)

  def push[O1 <: Operand, OpEn](p1: O1)(implicit ev: PUSH_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format)

  def pop[O1 <: Operand, OpEn](p1: O1)(implicit ev: POP_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format)

  def dec[O1 <: Operand, OpEn](p1: O1)(implicit ev: DEC_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format)

  def and[O1 <: Operand, O2 <: Operand, OpEn](p1: O1, p2: O2)(implicit ev: AND_2[OpEn, O1, O2], size: InstructionSize[O1,O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1,p2, size, format)

  def not[O1 <: Operand, OpEn](p1: O1)(implicit ev: NOT_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format)

  def lea[O1 <: Operand, O2 <: Operand, OpEn](p1: O1, p2: O2)(implicit ev: LEA_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format)

  def jmp[O1 <: Operand, OpEn](p1: O1)(implicit ev: JMP_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format)
  
  def mov[O1 <: Operand, O2 <: Operand, OpEn](p1: O1, p2: O2)(implicit ev: MOV_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format)

  def shr[O1 <: Operand, O2 <: Operand, OpEn](p1: O1, p2: O2)(implicit ev: SHR_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format)

  def jnz[O1 <: Operand, OpEn](p1: O1)(implicit ev: JNZ_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format)

  def jz[O1 <: Operand, OpEn](p1: O1)(implicit ev: JZ_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format)

  def int[O1 <: Operand, OpEn](p1: O1)(implicit ev: INT_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format)

  def shl[O1 <: Operand, O2 <: Operand, OpEn](p1: O1, p2: O2)(implicit ev: SHL_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format)

  def sbb[O1 <: Operand, O2 <: Operand, OpEn](p1: O1, p2: O2)(implicit ev: SBB_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format)

  def retn[O1 <: Operand, OpEn](p1: O1)(implicit ev: RETN_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format)

  def retn(implicit ev: RET) = ev.get
  
  def leave(implicit ev: LEAVE) =  ev.get

  def test[O1 <: Operand, O2 <: Operand, OpEn](p1: O1, p2: O2)(implicit ev: TEST_2[OpEn, O1, O2], format: TwoOperandFormat[OpEn, O1,O2]) = ev.get(p1, p2, format)

  def rdrand[O1 <: Operand, OpEn](p1: O1)(implicit ev: RDRAND_1[OpEn,O1], format: OneOperandFormat[OpEn, O1]) = ev.get(p1, format)
}