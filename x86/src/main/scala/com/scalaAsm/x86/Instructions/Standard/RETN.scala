package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import scala.annotation.implicitNotFound

abstract class RETN_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, OneOpcode]("RETN")

object RETN_1 {
  
  implicit object retn1 extends RETN_1[imm16, I] {
      def opcode = 0xC2
  }
}