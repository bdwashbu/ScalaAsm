package com.scalaAsm.x86
package Instructions
package Standard

object RET extends OperandInstruction[OneOpcode]("RET") {
  implicit object NearReturn extends ZeroOps {
      val opcode = OneOpcode(0xC3)
  }
}