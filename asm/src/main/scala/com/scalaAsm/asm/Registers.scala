package com.scalaAsm.asm

import com.scalaAsm.x86.x86Registers._
import com.scalaAsm.x86.{Immediate, RegisterOrMemory, Memory}
import com.scalaAsm.x86.DwordOperand

object Addressing {

  case class RegisterOffset[+T <: GPR, S <: Immediate](offset2: S, x: T) extends Memory {
     type Size = DwordOperand
     val reg = x
     val isMemory = true
     val offset = Some(offset2)
  }

  trait Addressable[X <: GPR] {
    self: X =>
    def -[Z <: Immediate {type X = Z }](offset: Z) = RegisterOffset[X, Z](offset.negate, this)
    def +[Z <: Immediate {type X = Z }](offset: Z) = RegisterOffset[X, Z](offset, this)
  }

  def *(x: GPR) = new Memory {
    type Size = DwordOperand
     val reg = x
     val isMemory = true
     val offset = None
  }
  
  def *(x: Memory) = new Memory {
    type Size = DwordOperand
     val reg = x.reg
     val isMemory = true
     val offset = x.offset
  }
  
  type +[A <: GPR, B <: Immediate] = RegisterOffset[A, B]
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