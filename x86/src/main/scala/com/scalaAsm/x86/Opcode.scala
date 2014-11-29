package com.scalaAsm.x86

import Operands.Memory.ModRM

sealed trait OpcodeFormat {
  def size: Int
  def get: Array[Byte]
  val opcodeExtension: Option[Byte] // some opcodes use the ModRM field
  def /+(x: Byte): OpcodeFormat
  def isOpcodePlus: Boolean
  def prefix: Seq[Prefix]
}

sealed trait RegType
case object rw extends RegType
case object rd extends RegType
case object rb extends RegType
case object ro extends RegType

case class OneOpcode(operand: Byte, prefix: Seq[Prefix]) extends OpcodeFormat {
  def get = Array(operand)
  val size = 1
  val opcodeExtension: Option[Byte] = None
  def /+(x: Byte) = new OneOpcode(operand, prefix) { override val opcodeExtension = Some(x) }
  def +(reg: RegType) = new OpcodePlus(operand, prefix)
  def isOpcodePlus = false
}

class OpcodePlus(opcode: Byte, prefix: Seq[Prefix]) extends OneOpcode(opcode, prefix) {
    var reg: ModRM.reg = _
	  override def get = Array((opcode + reg.ID).toByte)
	  override val size = 1
	  override val opcodeExtension: Option[Byte] = None
	  override def /+(x: Byte) = new OneOpcode(opcode, prefix) { override val opcodeExtension = Some(x) }
    override def isOpcodePlus = true
}

case class TwoOpcodes(opcode1: Byte, opcode2: Byte, prefix: Seq[Prefix]) extends OpcodeFormat {
  def get = Array(opcode1, opcode2)
  val size = 2
  val opcodeExtension: Option[Byte] = None
  def /+(x: Byte) = new TwoOpcodes(opcode1, opcode2, prefix) { override val opcodeExtension = Some(x) }
  def isOpcodePlus = false
}

