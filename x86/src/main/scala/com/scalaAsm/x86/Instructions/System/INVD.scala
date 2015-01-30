package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Invalidate Internal Caches
// Category: general

object INVD extends InstructionDefinition[OneOpcode]("INVD") with INVDImpl

trait INVDImpl {
  implicit object INVD_0 extends INVD._0 {
    def opcode = 0x8
  }
}
