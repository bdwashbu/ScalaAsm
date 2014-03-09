package com.scalaAsm.asm

import com.scalaAsm.x86.x86Registers._
import com.scalaAsm.x86.{Immediate, RegisterOrMemory}
import com.scalaAsm.x86.DwordOperand

object Addressing {

  case class RegisterOffset[+T <: Register with RegisterID, S <: Immediate](offset2: S, x: T) extends RegisterOrMemory {
     type Size = DwordOperand
     val reg = x
     val isMemory = true
     val offset = Some(offset2)
  }

  trait Addressable[X <: Register with RegisterID] {
    self: X =>
    def -[Z <: Immediate {type X = Z }](offset: Z) = RegisterOffset[X, Z](offset.negate, this)
    def +[Z <: Immediate {type X = Z }](offset: Z) = RegisterOffset[X, Z](offset, this)
  }

  case class *[+A <: RegisterOrMemory](x: A) extends RegisterOrMemory {
    type Size = DwordOperand
     val reg = x.reg
     val isMemory = true
     val offset = x.offset
  }
  type +[A <: Register with RegisterID, B <: Immediate] = RegisterOffset[A, B]
}

trait Registers {
  import Addressing._
  
    object rdi extends RDI with Addressable[RDI]
    object rax extends RAX with Addressable[RAX]
    object rcx extends RCX with Addressable[RCX]
    object rbp extends RBP with Addressable[RBP]
    object rdx extends RDX with Addressable[RDX]
    object rsp extends RSP with Addressable[RSP]
  
    object edi extends rdi.EDI with Addressable[rdi.EDI]
    object eax extends rax.EAX with Addressable[rax.EAX]
    object ecx extends rcx.ECX with Addressable[rcx.ECX]
    object ebp extends rbp.EBP with Addressable[rbp.EBP]
    object edx extends rdx.EDX with Addressable[rdx.EDX]
    object esp extends rsp.ESP with Addressable[rsp.ESP]
    
    object ax extends eax.AX with Addressable[eax.AX]
    object cx extends ecx.CX with Addressable[ecx.CX]
    object dx extends edx.DX with Addressable[edx.DX]
    
    object ah extends ax.AH with Addressable[ax.AH]
  
    object cl extends cx.CL with Addressable[cx.CL]
    
    object es extends ES
    object cs extends CS
    object ss extends SS
    object ds extends DS
    
   
}