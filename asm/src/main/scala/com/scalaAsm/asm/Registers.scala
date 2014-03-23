package com.scalaAsm.asm

import com.scalaAsm.x86.Operands.x86Registers._
import com.scalaAsm.x86.Operands.{Immediate, RegisterOrMemory, Memory}
import com.scalaAsm.x86.Operands.DwordOperand
import com.scalaAsm.x86.Operands.GPR

object Addressing {

  case class RegisterOffset[S <: Immediate, +T <: GPR](offset2: S, reg: T) extends Memory {
     type Size = DwordOperand
     val base = Some(reg)
     val offset = Some(offset2)
  }

  trait Addressable[X <: GPR] {
    self: X =>
    def -[Z <: Immediate {type X = Z }](offset: Z) = RegisterOffset[Z,X](offset.negate, this)
    def +[Z <: Immediate {type X = Z }](offset: Z) = RegisterOffset[Z,X](offset, this)
  }

  def *(gpr: GPR) = new Memory {
    type Size = gpr.Size
     val base = Some(gpr)
     val offset = None
  }
  
  def *(mem: Memory) = new Memory {
    type Size = mem.Size
     val base = mem.base
     val offset = mem.offset
  }
  
  def *(imm: Immediate) = new Memory {
    type Size = imm.Size
     val base = None
     val offset = Some(imm)
  }
  
  type +[A <: Immediate, B <: GPR] = RegisterOffset[A, B]
}

trait Registers {
  import Addressing._
  
    object rdi extends RDI with Addressable[RDI]
    object rax extends RAX with Addressable[RAX]
    object rcx extends RCX with Addressable[RCX]
    object rbp extends RBP with Addressable[RBP]
    object rdx extends RDX with Addressable[RDX]
    object rsp extends RSP with Addressable[RSP]
  
    object edi extends EDI with Addressable[EDI]
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
    
    object es extends ES
    object cs extends CS
    object ss extends SS
    object ds extends DS
    
   
}