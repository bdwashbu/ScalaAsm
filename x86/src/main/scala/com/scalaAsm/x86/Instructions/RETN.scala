package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._
import scala.annotation.implicitNotFound

abstract class RETN extends x86Instruction("RETN")

trait RETN_1[-O1] extends RETN with OneOperandInstruction[O1]

object RETN {
  
  implicit object retn1 extends RETN_1[imm16] {
      def operands = I()
      val opcode: OpcodeFormat = 0xC2
  }
}