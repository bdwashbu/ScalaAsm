package com.scalaAsm.x86
package Instructions
package Standard

abstract class NOT_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, OneOpcode]("NOT")

object NOT_1 {
  
  implicit object not1 extends NOT_1[rm32, M] {
      def opcode = 0xF7 /+ 2
  }
}