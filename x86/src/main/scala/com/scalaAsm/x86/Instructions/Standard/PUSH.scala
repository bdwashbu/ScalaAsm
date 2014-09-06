package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import scala.annotation.implicitNotFound

trait PUSH extends x86Instruction {
  val mnemonic = "PUSH"
}

@implicitNotFound(msg = "Cannot find PUSH implementation for ${O1}")
trait PUSH_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, OneOpcode] with PUSH

trait POWLow {
    
  implicit object LowPush extends PUSH_1[rm, M] {
      val opcode = 0xFF /+ 6
  }
  
  implicit object push3 extends PUSH_1[r16, O] {
      val opcode = OneOpcode(0x50) + rw
  }
}

object PUSH extends POWLow {

  implicit object push1 extends PUSH_1[r64, O] {
      val opcode = OneOpcode(0x50) + rd
      override val defaultsTo64Bit = true
  }
  
  implicit object push2 extends PUSH_1[r32, O] {
      val opcode = OneOpcode(0x50) + rd 
  }
  
  implicit object push4 extends PUSH_1[imm8, I] {
      val opcode = OneOpcode(0x6A)
  }
  
  implicit object push5 extends PUSH_1[imm16, I] {
      val opcode = OneOpcode(0x68)
  }
  
  implicit object push6 extends PUSH_1[imm32, I] {
      val opcode = OneOpcode(0x68)
  }
  
  implicit object push7 extends PUSH_1[CS, CSFormat] {
      val opcode = OneOpcode(0x0E)
  }
}