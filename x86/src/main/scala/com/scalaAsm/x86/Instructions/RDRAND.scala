package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._
import scala.annotation.implicitNotFound

abstract class RDRAND extends x86Instruction("RDRAND")

trait RDRAND_1[-O1 <: Operand] extends RDRAND with OneOperandInstruction[O1]

object RDRAND {
  
  implicit object rdrand1 extends RDRAND_1[rm32] {
      def opEn = M
      val opcode = (0x0F, 0xC7) /+ 6
  }
  
  implicit object rdrand2 extends RDRAND_1[rm16] {
      def opEn = M
      val opcode = (0x0F, 0xC7) /+ 6
  }
}