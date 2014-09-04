package com.scalaAsm.x86
package Instructions
package Standard

import scala.annotation.implicitNotFound

trait RDRAND extends x86Instruction {
  val mnemonic = "RDRAND"
}

trait RDRAND_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn] with RDRAND

object RDRAND {
  
  implicit object rdrand1 extends RDRAND_1[rm32, M] {
      val opcode = (0x0F, 0xC7) /+ 6
  }
  
  implicit object rdrand2 extends RDRAND_1[rm16, M] {
      val opcode = (0x0F, 0xC7) /+ 6
  }
}