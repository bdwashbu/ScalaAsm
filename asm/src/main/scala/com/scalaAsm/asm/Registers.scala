package com.scalaAsm.asm

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86._

trait Registers[Mode <: x86Mode] {

    trait RegisterOperand[X] {
      val reg: GeneralPurpose[X]
      override def toString = reg.toString
      def -[Z: x86Size](offset: Constant[Z]) = new reg.BaseIndex(offset.negate) {}
      def +[Z: x86Size](offset: Constant[Z]) = new reg.BaseIndex(offset) {}
    }

    object rdi extends RDI with RegisterOperand[_64]{val reg = new RDI}
    object rax extends RAX with RegisterOperand[_64]{val reg = new RAX}
    object rcx extends RCX with RegisterOperand[_64]{val reg = new RCX}
    object rbp extends RBP with RegisterOperand[_64]{val reg = new RBP}
    object rdx extends RDX with RegisterOperand[_64]{val reg = new RDX}
    object rbx extends RBX with RegisterOperand[_64]{val reg = new RBX}
    object rsp extends RSP with RegisterOperand[_64]{val reg = new RSP}
  
    object edi extends EDI with RegisterOperand[_32]{val reg = new EDI}
    object eax extends EAX with RegisterOperand[_32]{val reg = new EAX}
    object ecx extends ECX with RegisterOperand[_32]{val reg = new ECX}
    object ebp extends EBP with RegisterOperand[_32]{val reg = new EBP}
    object edx extends EDX with RegisterOperand[_32]{val reg = new EDX}
    object ebx extends EBX with RegisterOperand[_32]{val reg = new EBX}
    object esp extends ESP with RegisterOperand[_32]{val reg = new ESP}
  
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