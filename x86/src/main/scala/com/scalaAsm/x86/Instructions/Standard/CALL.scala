package com.scalaAsm.x86
package Instructions
package Standard

object CALL extends InstructionDefinition[OneOpcode]("CALL") with CALLHigh

  trait CALLLow {
    implicit object call2 extends CALL._1[rm32, M] {
      def opcode = 0xFF /+ 2
    }
  }
  
  trait CALLHigh extends CALLLow {
  
    implicit object call3 extends CALL._1[rel32, M] {
      def opcode = 0xE8
    }
  
    implicit object call1 extends CALL._1[rm16, M] {
      def opcode = 0xFF /+ 2
    }
  }