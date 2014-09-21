package com.scalaAsm.x86
package Instructions
package Standard

import scala.annotation.implicitNotFound

abstract class RDRAND_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, TwoOpcodes]("RDRAND")

object RDRAND_1 {
  
  implicit object rdrand1 extends RDRAND_1[rm32, M] {
      def opcode = (0x0F, 0xC7) /+ 6
  }
  
  implicit object rdrand2 extends RDRAND_1[rm16, M] {
      def opcode = (0x0F, 0xC7) /+ 6
  }
}