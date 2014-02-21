package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._
import scala.annotation.implicitNotFound

trait RDRAND

trait RDRAND_1[-O1] extends RDRAND {
  def get(p1: O1): Instruction
}

object RDRAND extends ModRM {
  
  implicit object rdrand1 extends RDRAND_1[rm32] {
    def get(x: rm32) = new M(x) {
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended1(this))
       val opcode = 0x0F.toByte
     val opcode2 = Some(0xC7.toByte)
     val opcodeExtension = Some(6.toByte)
    }
  }
  
  implicit object rdrand2 extends RDRAND_1[rm16] {
    def get(x: rm16) = new M(x) {
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended1(this))
       val opcode = 0x0F.toByte
     val opcode2 = Some(0xC7.toByte)
     val opcodeExtension = Some(6.toByte)
     }
  }
}