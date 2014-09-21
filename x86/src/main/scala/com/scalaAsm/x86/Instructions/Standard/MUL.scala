package com.scalaAsm.x86
package Instructions
package Standard

abstract class MUL_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, OneOpcode]("MUL")

object MUL_1 {
  
  implicit object mul1 extends MUL_1[rm32, M] {
      def opcode = 0xF7 /+ 4
  }
}