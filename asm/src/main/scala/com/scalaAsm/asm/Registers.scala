package com.scalaAsm.asm

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.x86.Operands.Memory.AddressingMode
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.x86.`package`.OperandSize

trait Registers {

  

  def *[A <: GPR, B <: OperandSize](mem: A#BaseIndex[B]): Operand[_,A#BaseIndex[B]] = Op(mem)

  def *[C <: OperandSize](offset: Constant[C])(implicit abs: AbsoluteAddress[C]): AbsoluteAddress[C] = { abs.offset = offset.value; abs }

  def *[X <: OperandSize](gpr: GeneralPurpose[X]) = Op(new gpr.Indirect {})
  
  object rdi extends RDI with Operand[RDI, RDI] {def get = new RDI{}}
  object rax extends RAX with Operand[RAX, RAX] {def get = this}
  object rcx extends RCX with Operand[RCX, RCX] {def get = this}
  object rbp extends RBP with Operand[RBP, RBP] {def get = this}
  object rdx extends RDX with Operand[RDX, RDX] {def get = this}
  object rbx extends RBX with Operand[RBX, RBX] {def get = this}
  object rsp extends RSP with Operand[RSP, RSP] {def get = this}

  object edi extends EDI with Operand[EDI, EDI] {def get = this}
  object ebx extends EBX with Operand[EBX, EBX] {def get = this}
  object eax extends EAX with Operand[EAX, EAX] {def get = this}
  object ecx extends ECX with Operand[ECX, ECX] {def get = this}
  object ebp extends EBP with Operand[EBP, EBP] {def get = this}
  object edx extends EDX with Operand[EDX, EDX] {def get = this}
  object esp extends ESP with Operand[ESP, ESP] {def get = this}

  object ax extends AX with Operand[AX, AX] {def get = this}
  object cx extends CX with Operand[CX, CX] {def get = this}
  object dx extends DX with Operand[DX, DX] {def get = this}

  object ah extends AH with Operand[AH, AH] {def get = this}

  object cl extends CL with Operand[CL, CL] {def get = this}

  object spl extends SPL with Operand[SPL, SPL] {def get = this}

  object es extends ES with Operand[ES, ES] {def get = this}
  object cs extends CS with Operand[CS, CS] {def get = this}
  object ss extends SS with Operand[SS, SS] {def get = this}
  object ds extends DS with Operand[DS, DS] {def get = this}

  object r8 extends R8 with Operand[R8, R8] {def get = this}
  object r9 extends R9 with Operand[R9, R9] {def get = this}
  object r10 extends R10 with Operand[R10, R10] {def get = this}
  object r11 extends R11 with Operand[R11, R11] {def get = this}
  object r12 extends R12 with Operand[R12, R12] {def get = this}
  object r13 extends R13 with Operand[R13, R13] {def get = this}
  object r14 extends R14 with Operand[R14, R14] {def get = this}
  object r15 extends R15 with Operand[R15, R15] {def get = this}
}