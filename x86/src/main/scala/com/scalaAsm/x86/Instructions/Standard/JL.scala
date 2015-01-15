package com.scalaAsm.x86
package Instructions
package Standard

object JL extends InstructionDefinition[OneOpcode]("JL") with jlLow
 
trait jlLow {
  
  implicit object jl1 extends JL._1[rel8] {
      def opcode = 0x7C
  }
}