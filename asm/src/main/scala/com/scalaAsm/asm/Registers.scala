package com.scalaAsm.asm

import com.scalaAsm.x86.Operands._

trait Registers[Mode <: x86Mode] {

    trait RegisterOperand[X <: GeneralPurpose[_]] {
      val reg: X
      override def toString = reg.toString
      def -[Z: x86Size](offset: Constant[Z]) = reg.getBaseIndex(offset.negate)
      def +[Z: x86Size](offset: Constant[Z]) = reg.getBaseIndex(offset)
    }

    object rdi extends RDI with RegisterOperand[RDI]{val reg = new RDI}
    object rax extends RAX with RegisterOperand[RAX]{val reg = new RAX}
    object rcx extends RCX with RegisterOperand[RCX]{val reg = new RCX}
    object rbp extends RBP with RegisterOperand[RBP]{val reg = new RBP}
    object rdx extends RDX with RegisterOperand[RDX]{val reg = new RDX}
    object rbx extends RBX with RegisterOperand[RBX]{val reg = new RBX}
    object rsp extends RSP with RegisterOperand[RSP]{val reg = new RSP}
  
    object edi extends EDI with RegisterOperand[EDI]{val reg = new EDI}
    object eax extends EAX with RegisterOperand[EAX]{val reg = new EAX}
    object ecx extends ECX with RegisterOperand[ECX]{val reg = new ECX}
    object ebp extends EBP with RegisterOperand[EBP]{val reg = new EBP}
    object edx extends EDX with RegisterOperand[EDX]{val reg = new EDX}
    object ebx extends EBX with RegisterOperand[EBX]{val reg = new EBX}
    object esp extends ESP with RegisterOperand[ESP]{val reg = new ESP}
  
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
    
    object xmm0 extends XMM0
    object xmm1 extends XMM1
    object xmm2 extends XMM2
    object xmm3 extends XMM3
    object xmm4 extends XMM4
    object xmm5 extends XMM5
    object xmm6 extends XMM6
    object xmm7 extends XMM7
}