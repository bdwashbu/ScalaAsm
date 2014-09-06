package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import scala.annotation.implicitNotFound

trait RETN extends x86Instruction {
  val mnemonic = "RETN"
}

trait RETN_1[-O1, OpEn <: OneOperandEncoding[O1]] extends OneOperandInstruction[O1, OpEn, OneOpcode] with RETN

object RETN {
  
  implicit object retn1 extends RETN_1[imm16, I] {
      val opcode = OneOpcode(0xC2)
  }
}