package com.scalaAsm.x86

import com.scalaAsm.x86.ModRM._
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._

object Instruction extends ModRM {

}

private[x86] trait Instruction extends ModRM {
  def opcode: Byte
  def opcode2: Option[Byte]
  val operands: OperandFormat
  def opcodeExtension: Option[Byte]
  def modRM: Option[AddressingFormSpecifier]

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
    val opCodes = List(Some(opcode), opcode2)
    opCodes.flatten.toArray ++ (modRM match {
      case Some(modRM) => modRM.getBytes
      case _ => Array.emptyByteArray
    })
  }
}

private[x86] trait Instruction1[X] extends Instruction {
  def operand1: X
}

private[x86] trait Instruction2[X, Y] extends Instruction {
  def operand1: X
  def operand2: Y
}