package com.scalaAsm.x86

import com.scalaAsm.x86.ModRM._
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._

private[x86] trait Instruction extends ModRM {
  def opcode: Opcodes
  val operands: OperandFormat
  val opcodeExtension: Option[Byte]
  def modRM: Option[AddressingFormSpecifier]
  
  implicit def toByte(x:Int) = x.toByte

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

trait Opcodes {
  def get: Array[Byte]
}

case class OneOpcode(operand1:Byte) extends Opcodes {
  def get = Array(operand1)
}

case class TwoOpcodes(operand1:Byte, operand2:Byte) extends Opcodes {
  def get = Array(operand1, operand2)
}