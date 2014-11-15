package com.scalaAsm.x86
package Instructions
package Standard

object MUL extends InstructionDefinition[OneOpcode]("MUL") with mulLow
 
trait mulLow {
  implicit object mul1 extends MUL._1[rm32, M] {
      def opcode = 0xF7 /+ 4
  }
}