package com.scalaAsm.asm

import com.scalaAsm.x86.Operands._

trait Registers[Mode <: x86Mode] {

//    trait RegisterOps[X <: GeneralPurpose[_]] {
//      self: Operand[X] =>
//      def -[Z: x86Size](offset: Operand[Constant[Z]]) = get.getBaseIndex(offset.get.negate)
//      def +[Z: x86Size](offset: Operand[Constant[Z]]) = get.getBaseIndex(offset.get)
//    }
    
    case class RegisterOperand[X <: GeneralPurpose[_]](reg: X) extends Operand[X] {
      def apply = reg
      override def toString = reg.toString
      def -[Z: x86Size](offset: Operand[Constant[Z]]) = reg.getBaseIndex(offset().negate)
      def +[Z: x86Size](offset: Operand[Constant[Z]]) = reg.getBaseIndex(offset())
    }

    def rdi(implicit ev: Mode <:< x86_64) = RegisterOperand(new RDI)
    def rax(implicit ev: Mode <:< x86_64) = RegisterOperand(new RAX)
    def rcx(implicit ev: Mode <:< x86_64) = RegisterOperand(new RCX)
    def rbp(implicit ev: Mode <:< x86_64) = RegisterOperand(new RBP)
    def rdx(implicit ev: Mode <:< x86_64) = RegisterOperand(new RDX)
    def rbx(implicit ev: Mode <:< x86_64) = RegisterOperand(new RBX)
    def rsp(implicit ev: Mode <:< x86_64) = RegisterOperand(new RSP)
  
    def edi(implicit ev: Mode <:< x86_32) = RegisterOperand(new EDI)
    def ebx(implicit ev: Mode <:< x86_32) = RegisterOperand(new EBX)
    def eax(implicit ev: Mode <:< x86_32) = RegisterOperand(new EAX)
    def ecx(implicit ev: Mode <:< x86_32) = RegisterOperand(new ECX)
    def ebp(implicit ev: Mode <:< x86_32) = RegisterOperand(new EBP)
    def edx(implicit ev: Mode <:< x86_32) = RegisterOperand(new EDX)
    def esp(implicit ev: Mode <:< x86_32) = RegisterOperand(new ESP)
  
    object ax extends AX with Operand[AX] {def apply = this}
    object cx extends CX with Operand[CX] {def apply = this}
    object dx extends DX with Operand[DX] {def apply = this}
  
    object ah extends AH with Operand[AH] {def apply = this}
  
    object cl extends CL with Operand[CL] {def apply = this}
  
    object spl extends SPL with Operand[SPL] {def apply = this}
  
    object es extends ES with Operand[ES] {def apply = this}
    object cs extends CS with Operand[CS] {def apply = this}
    object ss extends SS with Operand[SS] {def apply = this}
    object ds extends DS with Operand[DS] {def apply = this}
  
    object r8 extends R8 with Operand[R8] {def apply = this}
    object r9 extends R9 with Operand[R9] {def apply = this}
    object r10 extends R10 with Operand[R10] {def apply = this}
    object r11 extends R11 with Operand[R11] {def apply = this}
    object r12 extends R12 with Operand[R12] {def apply = this}
    object r13 extends R13 with Operand[R13] {def apply = this}
    object r14 extends R14 with Operand[R14] {def apply = this}
    object r15 extends R15 with Operand[R15] {def apply = this}
    
    object xmm0 extends XMM0 with Operand[XMM0] {def apply = this}
    object xmm1 extends XMM1 with Operand[XMM1] {def apply = this}
    object xmm2 extends XMM2 with Operand[XMM2] {def apply = this}
    object xmm3 extends XMM3 with Operand[XMM3] {def apply = this}
    object xmm4 extends XMM4 with Operand[XMM4] {def apply = this}
    object xmm5 extends XMM5 with Operand[XMM5] {def apply = this}
    object xmm6 extends XMM6 with Operand[XMM6] {def apply = this}
    object xmm7 extends XMM7 with Operand[XMM7] {def apply = this}
}