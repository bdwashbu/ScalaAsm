package com.scalaAsm.x86.Instructions

trait Catalog {

  def callNear[O1 <: Operand](p1: O1)(implicit ev: CALL_1[O1]) = ev(p1)

  def add[O1 <: Operand, O2 <: Operand](p1: O1, p2: O2)(implicit ev: ADD_2[O1, O2]) = ev(p1, p2)

  def or[O1 <: Operand, O2 <: Operand](p1: O1, p2: O2)(implicit ev: OR_2[O1, O2]) = ev(p1, p2)
  
  def sub[O1 <: Operand, O2 <: Operand](p1: O1, p2: O2)(implicit ev: SUB_2[O1, O2]) = ev(p1, p2)

  def mul[O1 <: Operand](p1: O1)(implicit ev: MUL_1[O1]) = ev(p1)

  def push[O1 <: Operand](p1: O1)(implicit ev: PUSH_1[O1]) = ev(p1)

  def pop[O1 <: Operand](p1: O1)(implicit ev: POP_1[O1]) = ev(p1)

  def dec[O1 <: Operand](p1: O1)(implicit ev: DEC_1[O1]) = ev(p1)

  def and[O1 <: Operand, O2 <: Operand](p1: O1, p2: O2)(implicit ev: AND_2[O1, O2]) = ev(p1,p2)

  def not[O1 <: Operand](p1: O1)(implicit ev: NOT_1[O1]) = ev(p1)

  def lea[O1 <: Operand, O2 <: Operand](p1: O1, p2: O2)(implicit ev: LEA_2[O1, O2]) = ev(p1, p2)

  def jmp[O1 <: Operand](p1: O1)(implicit ev: JMP_1[O1]) = ev(p1)
  
  def mov[O1 <: Operand, O2 <: Operand](p1: O1, p2: O2)(implicit ev: MOV_2[O1, O2]) = ev(p1, p2)

  def shr[O1 <: Operand, O2 <: Operand](p1: O1, p2: O2)(implicit ev: SHR_2[O1, O2]) = ev(p1, p2)

  def jnz[O1 <: Operand](p1: O1)(implicit ev: JNZ_1[O1]) = ev(p1)

  def jz[O1 <: Operand](p1: O1)(implicit ev: JZ_1[O1]) = ev(p1)

  def int[O1 <: Operand](p1: O1)(implicit ev: INT_1[O1]) = ev(p1)

  def shl[O1 <: Operand, O2 <: Operand](p1: O1, p2: O2)(implicit ev: SHL_2[O1, O2]) = ev(p1, p2)

  def sbb[O1 <: Operand, O2 <: Operand](p1: O1, p2: O2)(implicit ev: SBB_2[O1, O2]) = ev(p1, p2)

  def retn[O1 <: Operand](p1: O1)(implicit ev: RETN_1[O1]) = ev(p1)

  def retn(implicit ev: RET) = ev.apply
  
  def leave(implicit ev: LEAVE) =  ev.apply

  def test[O1 <: Operand, O2 <: Operand](p1: O1, p2: O2)(implicit ev: TEST_2[O1, O2]) = ev(p1, p2)

  def rdrand[O1 <: Operand](p1: O1)(implicit ev: RDRAND_1[O1]) = ev(p1)
}