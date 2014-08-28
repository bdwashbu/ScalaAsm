package com.scalaAsm.asm

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.x86.Operands.Memory.AddressingMode
import com.scalaAsm.x86.Operands.Memory.RegisterIndirect
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.x86.`package`.OperandSize

trait Registers {

  def *[X <: OperandSize](gpr: GeneralPurpose[X]) = new RegisterIndirect[GeneralPurpose[X]](gpr) {
    type Size = X
  }

  def *[M <: AddressingMode](mem: M): M = mem

  def *[C <: Constant[C]](offset: C)(implicit abs: AbsoluteAddress[C]): AbsoluteAddress[C] = { abs.offset = offset.value; abs }

  object rdi extends RDI
  object rax extends RAX
  object rcx extends RCX
  object rbp extends RBP
  object rdx extends RDX
  object rbx extends RBX
  object rsp extends RSP

  object edi extends EDI
  object ebx extends EBX
  object eax extends EAX
  object ecx extends ECX
  object ebp extends EBP
  object edx extends EDX
  object esp extends ESP

  object ax extends AX
  object cx extends CX
  object dx extends DX

  object ah extends AH

  object cl extends CL

  object spl extends SPL

  object es extends ES
  object cs extends CS
  object ss extends SS
  object ds extends DS

  object r8 extends R8
  object r9 extends R9
  object r10 extends R10
  object r11 extends R11
  object r12 extends R12
  object r13 extends R13
  object r14 extends R14
  object r15 extends R15
}