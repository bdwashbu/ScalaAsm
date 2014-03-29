package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._
import scala.annotation.implicitNotFound

abstract class PUSH extends x86Instruction("PUSH")

@implicitNotFound(msg = "Cannot find PUSH implementation for ${O1}")
trait PUSH_1[-O1] extends PUSH with OneOperandInstruction[O1] with InstructionFormat

trait POWLow {
    
  implicit object highPush extends PUSH_1[rm] {
      def operands = M()
      val opcode = 0xFF /+ 6
  }
}

object PUSH extends POWLow {

  implicit object push2 extends PUSH_1[r32] {
      def operands = O()
      val opcode = OpcodePlusRd(0x50)
  }
  
  implicit object push3 extends PUSH_1[r16] {
      def operands = O()
      val opcode = OpcodePlusRd(0x50)
  }
  
  implicit object push4 extends PUSH_1[imm8] {
      def operands = I()
      val opcode: OpcodeFormat = 0x6A
  }
  
  implicit object push5 extends PUSH_1[imm16] {
      def operands = I()
      val opcode: OpcodeFormat = 0x68
  }
  
  implicit object push6 extends PUSH_1[imm32] {
      def operands = I()
      val opcode: OpcodeFormat = 0x68
  }
  
  implicit object push7 extends PUSH_1[CS] {
      def operands = new OneOperandFormat[CS]() {def getAddressingForm(op1: CS, opcode: OpcodeFormat) = null}
      val opcode: OpcodeFormat = 0x0E
  }
}