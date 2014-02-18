package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._
import scala.annotation.implicitNotFound

trait PUSH extends ModRM

@implicitNotFound(msg = "Cannot find PUSH implementation for ${O1}")
trait PUSH_1[-O1] extends PUSH {
  def get(op1: O1): Instruction
}

trait POWLow extends ModRM {
  
  abstract class O[X <: OperandSize](op1: ModRM.reg[X]) extends Instruction1[ModRM.reg[X]] {
    val opcodeExtension = None
    val opcode = (0x50 + op1.ID).toByte
    val opcode2 = None
    val operand1 = op1
    val modRM: Option[AddressingFormSpecifier] = None
  }
  
  abstract class I[X <: Immediate](op1: X) extends Instruction1[X] {
    val opcodeExtension = None
    val operand1 = op1
    val opcode2 = None
  }
  
  abstract class M[X <: OperandSize](op1: ModRM.rm[X]) extends Instruction1[ModRM.rm[X]] {
    val opcodeExtension = Some(6.toByte)
    val opcode = 0xFF.toByte
    val operand1 = op1
    val opcode2 = None
  }
  
  implicit object push6 extends PUSH_1[rm32] {
    def get(x: rm32) = new M[DwordOperand](x) {
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended1(this))
    }
  }
  
  //implicit object push6 extends PUSH_M[rm32] { def get(x: rm32) = 0xFF.toByte +: modRMExtended(x, extensionCode = 6.toByte).getBytes }  
}

object PUSH extends POWLow {
  
  implicit object push1 extends PUSH_1[r32] {
    def get(x: r32) = new O[DwordOperand](x) {}
  }
  
  implicit object push8 extends PUSH_1[r16] {
    def get(x: r16) = new O[WordOperand](x) {}
  }
  
  implicit object push4 extends PUSH_1[imm8] {
    def get(x: imm8) = new I[imm8](x) {
      val opcode = 0x6A.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm1(this))
    }
  }
  
  implicit object push5 extends PUSH_1[imm16] {
    def get(x: imm16) = new I[imm16](x) {
      val opcode = 0x68.toByte
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm1(this))
    }
  }
  
  implicit object push7 extends PUSH_1[CS] {
    def get(x: CS) = new Instruction1[CS] {
      val opcodeExtension = None
      val opcode = 0x0E.toByte
      val opcode2 = None
      val operand1 = x
      val modRM: Option[AddressingFormSpecifier] = None
    }
  }
}