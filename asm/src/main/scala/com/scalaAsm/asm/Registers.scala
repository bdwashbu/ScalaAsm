package com.scalaAsm.asm

import com.scalaAsm.x86.Operands._

trait Registers[Mode <: x86Mode] {


    def rdi(implicit ev: Mode <:< x86_64) = new RDI with Operand[RDI, RDI] {def get = this}
    def rax(implicit ev: Mode <:< x86_64) = new RAX with Operand[RAX, RAX] {def get = this}
    def rcx(implicit ev: Mode <:< x86_64) = new RCX with Operand[RCX, RCX] {def get = this}
    def rbp(implicit ev: Mode <:< x86_64) = new RBP with Operand[RBP, RBP] {def get = this}
    def rdx(implicit ev: Mode <:< x86_64) = new RDX with Operand[RDX, RDX] {def get = this}
    def rbx(implicit ev: Mode <:< x86_64) = new RBX with Operand[RBX, RBX] {def get = this}
    def rsp(implicit ev: Mode <:< x86_64) = new RSP with Operand[RSP, RSP] {def get = this}
  
    def edi(implicit ev: Mode <:< x86_32) = new EDI with Operand[EDI, EDI] {def get = this}
    def ebx(implicit ev: Mode <:< x86_32) = new EBX with Operand[EBX, EBX] {def get = this}
    def eax(implicit ev: Mode <:< x86_32) = new EAX with Operand[EAX, EAX] {def get = this}
    def ecx(implicit ev: Mode <:< x86_32) = new ECX with Operand[ECX, ECX] {def get = this}
    def ebp(implicit ev: Mode <:< x86_32) = new EBP with Operand[EBP, EBP] {def get = this}
    def edx(implicit ev: Mode <:< x86_32) = new EDX with Operand[EDX, EDX] {def get = this}
    def esp(implicit ev: Mode <:< x86_32) = new ESP with Operand[ESP, ESP] {def get = this}
  
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