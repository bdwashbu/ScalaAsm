package com.scalaAsm.x86

import com.scalaAsm.x86.ModRM._

object Instruction extends ModRM with Operands {
  def newInst2[X, Y](inst: Instruction2[X,Y])(implicit ev: MODRM_2Extended[X, Y]): AddressingFormSpecifier = {
      modRM2Extended(inst.operand1, inst.operand2, inst.opcodeExtension)
  }

def newInst1[X](inst: Instruction1[X])(implicit ev: MODRM_1Extended[X]): AddressingFormSpecifier = {
      modRMExtended(inst.operand1, inst.opcodeExtension)
  }
}

private[x86] trait Instruction extends ModRM with Operands {
  def opcode: Byte
  def opcodeExtension: Byte
  def modRM: AddressingFormSpecifier
  def getBytes: Array[Byte] = {
    Array(opcode) ++ modRM.getBytes
  }
}

private[x86] trait Instruction1[X] extends Instruction {
  def operand1: X
  def getBytes[P](implicit ev: MODRM_1Extended[P]): Array[Byte] = {
    Array(opcode) ++ modRM.getBytes
  }
}

private[x86] trait Instruction2[X,Y] extends Instruction {
  def operand1: X
  def operand2: Y
  def getBytes[P,Q](implicit ev: MODRM_2Extended[P, Q]): Array[Byte] = {
    Array(opcode) ++ modRM.getBytes
  }
}