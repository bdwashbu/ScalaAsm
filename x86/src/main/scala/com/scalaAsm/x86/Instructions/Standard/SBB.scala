package com.scalaAsm.x86
package Instructions
package Standard

object SBB extends OperandInstruction[OneOpcode]("SBB") with sbbLow
 
trait sbbLow {

  implicit object sbb1 extends SBB.TwoOps[r32, rm32, RM] {
    def opcode = 0x1B
  }
}