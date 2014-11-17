package com.scalaAsm.x86
package Instructions
package Standard

object RET extends InstructionDefinition[OneOpcode]("RET") {
  implicit object NearReturn extends _0 {
      val opcode = OneOpcode(0xC3, Seq())
  }
}