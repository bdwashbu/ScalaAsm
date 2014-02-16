package com.scalaAsm.x86

import com.scalaAsm.x86.ModRM._

object Instruction extends ModRM with Operands {
  def newInst2[X, Y](inst: Instruction2[X,Y])(implicit ev: MODRM_2Extended[X, Y]): AddressingFormSpecifier = {
      modRM2Extended(inst.operand1.get, inst.operand2.get, inst.opcodeExtension)
  }

def newInst1[X](inst: Instruction1[X])(implicit ev: MODRM_1Extended[X]): AddressingFormSpecifier = {
      modRMExtended(inst.operand1.get, inst.opcodeExtension)
  }
}

private[x86] trait Instruction extends ModRM with Operands {
  var opcode: Byte = 0.toByte
  var modRM: Option[AddressingFormSpecifier] = None
  def getBytes: Array[Byte] = {
    Array(opcode) ++ modRM.get.getBytes
  }
}

private[x86] trait Instruction1[X] extends Instruction {
  var operand1: Option[X] = None
  var opcodeExtension: Byte = 0.toByte
  def getBytes[P](implicit ev: MODRM_1Extended[P]): Array[Byte] = {
    Array(opcode) ++ modRM.get.getBytes
  }
}

private[x86] trait Instruction2[X,Y] extends Instruction {
  var operand1: Option[X] = None
  var operand2: Option[Y] = None
  var opcodeExtension: Byte = 0.toByte
  def getBytes[P,Q](implicit ev: MODRM_2Extended[P, Q]): Array[Byte] = {
    Array(opcode) ++ modRM.get.getBytes
  }
}