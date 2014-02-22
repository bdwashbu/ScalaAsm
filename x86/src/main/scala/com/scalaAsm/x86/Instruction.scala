package com.scalaAsm.x86

import com.scalaAsm.x86.ModRM._
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._

private[x86] trait Instruction extends ModRM {
  def opcode: Opcodes
  val operands: OperandFormat
  def modRM: Option[AddressingFormSpecifier]
  
  implicit def toByte(x:Int) = x.toByte
  
  case class MI[M <: ModRM.reg, I <: imm](op1: M, op2: I) extends TwoOperands[M, I](op1, op2)
  
  case class RM[R <: ModRM.reg, M <: ModRM.rm](op1: R, op2: M) extends TwoOperands[R,M](op1, op2)
  
  case class O[R <: ModRM.reg](op1: R) extends OneOperand[R](op1)
  
  case class I[I <: Immediate](op1: I) extends OneOperand[I](op1)
  
  case class M[M <: ModRM.rm](op1: M) extends OneOperand[M](op1)
  
  case class M1[M <: ModRM.rm](op1: M) extends OneOperand[M](op1)

  def getAddressingFormExtended2[X, Y](ops: TwoOperands[X, Y], opcodeExtension: Byte)(implicit ev: MODRM_2Extended[X, Y]): AddressingFormSpecifier = {
    modRM2Extended(ops.operand1, ops.operand2, opcodeExtension)
  }

  def getAddressingFormExtended1[X](ops: OneOperand[X], opcodeExtension: Byte)(implicit ev: MODRM_1Extended[X]): AddressingFormSpecifier = {
    modRMExtended(ops.operand1, opcodeExtension)
  }
  
  def getAddressingForm2[X, Y](ops: TwoOperands[X, Y])(implicit ev: MODRM_2[X, Y]): AddressingFormSpecifier = {
    modRM2(ops.operand1, ops.operand2)
  }

  def getAddressingForm1[X](ops: OneOperand[X])(implicit ev: MODRM_1[X]): AddressingFormSpecifier = {
    modRM(ops.operand1)
  }

  def getBytes: Array[Byte] = {
    opcode.get ++ (modRM match {
      case Some(modRM) => modRM.getBytes
      case _ => Array.emptyByteArray
    })
  }
}

trait OpcodeExtension {
  val opcodeExtension: Byte
}

trait Opcodes {
  def get: Array[Byte]
}

case class OneOpcode(operand1:Byte) extends Opcodes {
  def get = Array(operand1)
  def / (x: Byte) = new OneOpcodeWithExtension(operand1, x)
}

class OneOpcodeWithExtension(operand1:Byte, val opcodeExtension: Byte) extends OneOpcode(operand1) with OpcodeExtension

case class TwoOpcodes(operand1:Byte, operand2:Byte) extends Opcodes {
  def get = Array(operand1, operand2)
  def / (x: Byte) = new TwoOpcodesWithExtension(operand1, operand2, x)
}

class TwoOpcodesWithExtension(operand1:Byte, operand2:Byte, val opcodeExtension: Byte) extends TwoOpcodes(operand1, operand2) with OpcodeExtension