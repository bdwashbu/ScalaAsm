package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._
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
    
  implicit object push6 extends PUSH_1[rm32] {
    def get(x: rm32) = new Instruction {
      val operands = M(x)
      val opcodeExtension = Some(6.toByte)
      val opcode = 0xFF.toByte
      val opcode2 = None
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingFormExtended1(operands, opcodeExtension.get))
    }
  }
}

object PUSH extends POWLow {
  
  implicit object push1 extends PUSH_1[r32] {
    def get(x: r32) = new Instruction {
      val operands = O(x)
      val opcode = (0x50 + x.ID).toByte
      val opcodeExtension = None
      val opcode2 = None
      val modRM: Option[AddressingFormSpecifier] = None
    }
  }
  
  implicit object push8 extends PUSH_1[r16] {
    def get(x: r16) = new Instruction {
      val operands = O(x)
      val opcodeExtension = None
      val opcode = (0x50 + x.ID).toByte
      val opcode2 = None
      val modRM: Option[AddressingFormSpecifier] = None
    }
  }
  
  implicit object push4 extends PUSH_1[imm8] {
    def get(x: imm8) = new Instruction {
      val operands = I[imm8](x)
      val opcode = 0x6A.toByte
      val opcode2 = None
      val opcodeExtension = None
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm1(operands))
    }
  }
  
  implicit object push5 extends PUSH_1[imm16] {
    def get(x: imm16) = new Instruction {
      val operands = I[imm16](x)
      val opcode = 0x68.toByte
      val opcode2 = None
      val opcodeExtension = None
      val modRM: Option[AddressingFormSpecifier] = Some(getAddressingForm1(operands))
    }
  }
  
  implicit object push7 extends PUSH_1[CS] {
    def get(x: CS) = new Instruction {
      val operands = new OneOperand[CS](x) {}
      val opcodeExtension = None
      val opcode = 0x0E.toByte
      val opcode2 = None
      val operand1 = x
      val modRM: Option[AddressingFormSpecifier] = None
    }
  }
}