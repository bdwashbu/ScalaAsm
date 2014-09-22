package com.scalaAsm.x86
package Instructions
package Standard

abstract class LEAVE extends ZeroOperandInstruction[OneOpcode]("LEAVE")

object LEAVE {
  
  implicit object lea1 extends LEAVE {
      def opcode = 0xC9
  }
}