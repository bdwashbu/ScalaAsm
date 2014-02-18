package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._
import scala.annotation.implicitNotFound

trait RDRAND

trait RDRAND_1[-O1] extends RDRAND {
  def get(p1: O1): Instruction
}

object RDRAND extends ModRM {
  
  abstract class M[X <: OperandSize](op1: ModRM.rm[X]) extends Instruction1[ModRM.rm[X]] {
     val opcodeExtension = Some(6.toByte)
     val operand1 = op1
  }
  
  implicit object rdrand1 extends RDRAND_1[rm32] {
    def get(x: rm32) = new M[DwordOperand](x) {
      val opcode = 0x0F.toByte
      val opcode2 = Some(0xC7.toByte)
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended1(this))
     }
  }
  
  implicit object rdrand2 extends RDRAND_1[rm16] {
    def get(x: rm16) = new M[WordOperand](x) {
      val opcode = 0x0F.toByte
      val opcode2 = Some(0xC7.toByte)
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended1(this))
     }
  }
  
  //implicit object rdrand1 extends RDRAND_M[rm32] { def get(x: rm32) = Array[Byte](0x0F.toByte, 0xC7.toByte) ++ modRMExtended(x, extensionCode = 6.toByte).getBytes }
  //implicit object rdrand2 extends RDRAND_M[rm16] { def get(x: rm16) = Array[Byte](0x0F.toByte, 0xC7.toByte) ++ modRMExtended(x, extensionCode = 6.toByte).getBytes }
  //implicit object rdrand3 extends RDRAND_M[r64] { def get(x: r32) = Array[Byte](0x0F.toByte, 0xC7.toByte) ++ modRM(x, reg = 6.toByte) }
}