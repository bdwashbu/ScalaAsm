package com.scalaAsm.x86
package Instructions
package Standard

object ADD extends InstructionDefinition[OneOpcode]("ADD") with AddLow

trait AddLow {
  implicit object ADD_1 extends ADD._2[16, 16] {
    def opcode = 0x1
    override def prefix = REX.W(true)
  }

  implicit object ADD_1 extends ADD._2[32, 32] {
    def opcode = 0x1
    override def prefix = REX.W(true)
  }

  implicit object ADD_1 extends ADD._2[64, 64] {
    def opcode = 0x1
    override def prefix = REX.W(true)
  }

  implicit object ADD_3 extends ADD._2[16, 16] {
    def opcode = 0x3
    override def prefix = REX.W(true)
  }

  implicit object ADD_3 extends ADD._2[32, 32] {
    def opcode = 0x3
    override def prefix = REX.W(true)
  }

  implicit object ADD_3 extends ADD._2[64, 64] {
    def opcode = 0x3
    override def prefix = REX.W(true)
  }

  implicit object ADD_131 extends ADD._2[16, 8] {
    def opcode = 0x83
    override def prefix = REX.W(true)
  }
  implicit object ADD_131 extends ADD._2[32, 8] {
    def opcode = 0x83
    override def prefix = REX.W(true)
  }
  implicit object ADD_131 extends ADD._2[64, 8] {
    def opcode = 0x83
    override def prefix = REX.W(true)
  }
}
