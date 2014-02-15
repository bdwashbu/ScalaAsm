package com.scalaAsm.x86

object Instruction extends ModRM with Operands {
  def newInst2[X, Y](opcode: Byte, operand1: X, operand2: Y, opcodeExtension: Byte = -1)(implicit ev: MODRM_2Extended[X, Y]): Instruction1 = {
    new Instruction1(
      opcode = 0x83.toByte,
      modRM2Extended(operand1, operand2, opcodeExtension))
  }

  def newInst1[X](opcode: Byte, operand1: X, opcodeExtension: Byte = -1)(implicit ev: MODRM_1Extended[X]): Instruction1 = {
    new Instruction1(
      opcode = 0x83.toByte,
      modRMExtended(operand1, opcodeExtension))
  }
}

private[x86] trait Instruction extends Operands

private[x86] case class Instruction1(opcode: Byte, modRM: AddressingFormSpecifier) extends Instruction {
  def getBytes: Array[Byte] = {
    Array(opcode) ++ modRM.getBytes
  }
}