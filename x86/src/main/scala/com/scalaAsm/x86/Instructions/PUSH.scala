package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.x86Registers._
import scala.annotation.implicitNotFound

abstract class PUSH extends x86Instruction("PUSH")

@implicitNotFound(msg = "Cannot find PUSH implementation for ${O1}")
trait PUSH_1[-O1] extends PUSH with OneOperand[O1] with OperandEncoding

trait POWLow {
    
  implicit object highPush extends PUSH_1[rm] {
      def operands = M(x)
      val opcode = 0xFF /+ 6
  }
}

object PUSH extends POWLow {
  
  implicit object push2 extends PUSH_1[r32] {
      def operands = O(x)
      def opcode = OpcodePlusRd(0x50, x)
  }
  
  implicit object push3 extends PUSH_1[r16] {
      def operands = O(x)
      def opcode = OpcodePlusRd(0x50, x)
  }
  
  implicit object push4 extends PUSH_1[imm8] {
      def operands = I[imm8](x)
      val opcode: Opcodes = 0x6A
  }
  
  implicit object push5 extends PUSH_1[imm16] {
      def operands = I[imm16](x)
      val opcode: Opcodes = 0x68
  }
  
  implicit object push6 extends PUSH_1[imm32] {
      def operands = I[imm32](x)
      val opcode: Opcodes = 0x68
  }
  
  implicit object push7 extends PUSH_1[CS] {
      def operands = new OneOperandFormat[CS](x) {def getAddressingForm = null}
      val opcode: Opcodes = 0x0E
  }
}