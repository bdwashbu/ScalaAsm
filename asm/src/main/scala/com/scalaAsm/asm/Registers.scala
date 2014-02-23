package com.scalaAsm.asm

import com.scalaAsm.x86.x86Registers._
import com.scalaAsm.x86.{Immediate, RegisterOrMemory}
import com.scalaAsm.x86.DwordOperand

object Addressing {

  case class RegisterOffset[+T <: Register, S <: Immediate](offset2: S, x: T) extends RegisterOrMemory {
     type Size = DwordOperand
     val reg = x
     val isMemory = true
     val offset = Some(offset2)
  }

  trait Addressable[X <: Register] {
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
  type +[A <: Register, B <: Immediate] = RegisterOffset[A, B]
}

trait Registers {
  import Addressing._
    val edi = new EDI with Addressable[EDI]
    val eax = new EAX with Addressable[EAX]
    val ecx = new ECX with Addressable[ECX]
    val ebp = new EBP with Addressable[EBP]
    val edx = new EDX with Addressable[EDX]
    val esp = new ESP with Addressable[ESP]
    
    val cl = new CL with Addressable[CL]
    
    val es = new ES with Addressable[ES]
    val cs = new CS with Addressable[CS]
    val ss = new SS with Addressable[SS]
    val ds = new DS with Addressable[DS]
    
    val ax = new AX with Addressable[AX]
    val cx = new CX with Addressable[CX]
    val dx = new DX with Addressable[DX]
    
    val ah = new AH with Addressable[AH]
}