package com.scalaAsm.x86
package Instructions
package Standard

abstract class RET extends ZeroOperandInstruction[OneOpcode]("RET")

object RET {

  implicit object NearReturn extends RET {
      val opcode = OneOpcode(0xC3)
  }
}