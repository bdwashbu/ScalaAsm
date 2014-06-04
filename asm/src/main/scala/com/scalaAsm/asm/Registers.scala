package com.scalaAsm.asm

import com.scalaAsm.x86.Operands._

object Addressing {

  case class RegisterOffset[S <: Displacement, +T <: GPR](offset2: S, reg: T) extends BaseIndex {
     type Size = DwordOperand
     def size = reg.size
     val base = reg
     val offset = offset2
  }

  trait Addressable[X <: GPR] {
    self: X =>
    def -[Z <: Displacement](offset: Z) = RegisterOffset(offset.negate, this)
    def +[Z <: Displacement](offset: Z) = RegisterOffset(offset, this)
  }

  def *(gpr: GPR) = new RegisterIndirect {
    type Size = gpr.Size
    def size = gpr.size
     val base = gpr
  }
  
  def *[M <: Memory](mem: M): M = mem
  
  def *(imm: Immediate) = new ImmediateMemory {
    type Size = imm.Size
    def size = imm.size
    def immediate = imm
  }

  type +[A <: Displacement, B <: GPR] = RegisterOffset[A, B]
}

trait Registers {
  import Addressing._
  
    object rdi extends RDI with Addressable[RDI]
    object rax extends RAX with Addressable[RAX]
    object rcx extends RCX with Addressable[RCX]
    object rbp extends RBP with Addressable[RBP]
    object rdx extends RDX with Addressable[RDX]
    object rbx extends RBX with Addressable[RBX]
    object rsp extends RSP with Addressable[RSP]
  
    object edi extends EDI with Addressable[EDI]
    object ebx extends EBX with Addressable[EBX]
    object eax extends EAX with Addressable[EAX]
    object ecx extends ECX with Addressable[ECX]
    object ebp extends EBP with Addressable[EBP]
    object edx extends EDX with Addressable[EDX]
    object esp extends ESP with Addressable[ESP]
    
    object ax extends AX with Addressable[AX]
    object cx extends CX with Addressable[CX]
    object dx extends DX with Addressable[DX]
    
    object ah extends AH with Addressable[AH]
  
    object cl extends CL with Addressable[CL]
  
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