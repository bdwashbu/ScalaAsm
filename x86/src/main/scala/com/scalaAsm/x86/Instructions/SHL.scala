package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand, ByteOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._

trait SHL

trait SHL_1[-O1] extends SHL {
  def get(x: O1): Instruction
}

trait SHL_2[-O1, -O2] extends SHL {
  def get(x: O1, y: O2): Instruction
}

object SHL extends ModRM {
  
  abstract class M1[M <: ModRM.rm](op1: M) extends Instruction1[M] {
     val operand1 = op1
     val operand2 = null
     val opcode2 = None
  }
  
  implicit object shl1 extends SHL_2[rm8, One] {
    def get(x: rm8, y: One) = new M1(x) {
      val opcode = 0xD0.toByte
      val opcodeExtension = Some(4.toByte)
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended1(this))
     }
  }
  
  //implicit object shl1 extends SHL_2[rm8, One] { def get(x: rm8) = 0xD0.toByte +: modRMExtended(x, extensionCode = 4.toByte).getBytes }
}