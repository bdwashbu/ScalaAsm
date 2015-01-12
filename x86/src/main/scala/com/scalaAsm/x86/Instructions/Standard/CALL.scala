package com.scalaAsm.x86
package Instructions
package Standard

object CALL extends InstructionDefinition[OneOpcode]("CALL") with CALLHigh

  trait CALLLow {
    implicit object call2 extends CALL._1_new[rm32] {
      def opcode = 0xFF /+ 2
    }
  }
  
  trait CALLHigh extends CALLLow {
  
    implicit object call3 extends CALL._1_new[rel32] {
      def opcode = 0xE8
    }
  
    implicit object call1 extends CALL._1_new[rm16] {
      def opcode = 0xFF /+ 2
    }
  }