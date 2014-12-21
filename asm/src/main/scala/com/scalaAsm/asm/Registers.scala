package com.scalaAsm.asm

import com.scalaAsm.x86.Operands._

trait Registers[Mode <: x86Mode] {

    trait RegisterOps[X <: GeneralPurpose[_]] {
      self: Operand[X] =>
      def -[Z: x86Size](offset: Operand[Constant[Z]]) = get.getBaseIndex(offset.get.negate)
      def +[Z: x86Size](offset: Operand[Constant[Z]]) = get.getBaseIndex(offset.get)
    }
    
    case class RegisterOperand[X <: GeneralPurpose[_]](reg: X) extends Operand[X] with RegisterOps[X] {
      def get = reg
      override def toString = reg.toString
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
  
    object ax extends AX with Operand[AX] {def get = this}
    object cx extends CX with Operand[CX] {def get = this}
    object dx extends DX with Operand[DX] {def get = this}
  
    object ah extends AH with Operand[AH] {def get = this}
  
    object cl extends CL with Operand[CL] {def get = this}
  
    object spl extends SPL with Operand[SPL] {def get = this}
  
    object es extends ES with Operand[ES] {def get = this}
    object cs extends CS with Operand[CS] {def get = this}
    object ss extends SS with Operand[SS] {def get = this}
    object ds extends DS with Operand[DS] {def get = this}
  
    object r8 extends R8 with Operand[R8] {def get = this}
    object r9 extends R9 with Operand[R9] {def get = this}
    object r10 extends R10 with Operand[R10] {def get = this}
    object r11 extends R11 with Operand[R11] {def get = this}
    object r12 extends R12 with Operand[R12] {def get = this}
    object r13 extends R13 with Operand[R13] {def get = this}
    object r14 extends R14 with Operand[R14] {def get = this}
    object r15 extends R15 with Operand[R15] {def get = this}
}