package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import scala.annotation.implicitNotFound

//class PUSH extends OperandInstruction[OneOpcode]("PUSH")
object PUSH extends OperandInstruction[OneOpcode]("PUSH") with PUSHHigh

trait PUSHLow {
    implicit object LowPush extends PUSH.OneOp[rm, M] {
        def opcode = 0xFF /+ 6
    }
    
    implicit object push3 extends PUSH.OneOp[r16, O] {
        def opcode = 0x50 + rw
    }
  }
  
 trait PUSHHigh extends PUSHLow {
    implicit object push1 extends PUSH.OneOp[r64, O] {
        def opcode = 0x50 + rd
        override val defaultsTo64Bit = true
    }
    
    implicit object push2 extends PUSH.OneOp[r32, O] {
        def opcode = 0x50 + rd 
    }
    
    implicit object push4 extends PUSH.OneOp[imm8, I] {
        def opcode = 0x6A
    }
    
    implicit object push5 extends PUSH.OneOp[imm16, I] {
        def opcode = 0x68
    }
    
    implicit object push6 extends PUSH.OneOp[imm32, I] {
        def opcode = 0x68
    }
    
    implicit object push7 extends PUSH.OneOp[CS, CSFormat] {
        def opcode = 0x0E
    }
  }

 