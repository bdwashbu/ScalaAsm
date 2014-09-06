package com.scalaAsm.x86

import Operands.Memory.ModRM

sealed trait OpcodeFormat {
  def size: Int
  def get: Array[Byte]
  val opcodeExtension: Option[Byte] // some opcodes use the ModRM field
  def /+(x: Byte): OpcodeFormat
}

sealed trait RegType
case object rw extends RegType
case object rd extends RegType
case object rb extends RegType

case class OneOpcode(operand1: Byte) extends OpcodeFormat {
  def get = Array(operand1)
  val size = 1
  val opcodeExtension: Option[Byte] = None
  def /+(x: Byte) = new OneOpcode(operand1) { override val opcodeExtension = Some(x) }
  def +(reg: RegType) = new OpcodePlus(operand1)
}

class OpcodePlus(opcode1: Byte) extends OneOpcode(opcode1) {
    var reg: ModRM.reg = _
	  override def get = Array((opcode1 + reg.ID).toByte)
	  override val size = 1
	  override val opcodeExtension: Option[Byte] = None
	  override def /+(x: Byte) = new OneOpcode(opcode1) { override val opcodeExtension = Some(x) }
}

case class TwoOpcodes(opcode1: Byte, opcode2: Byte) extends OpcodeFormat {
  def get = Array(opcode1, opcode2)
  val size = 2
  val opcodeExtension: Option[Byte] = None
  def /+(x: Byte) = new TwoOpcodes(opcode1, opcode2) { override val opcodeExtension = Some(x) }
}

