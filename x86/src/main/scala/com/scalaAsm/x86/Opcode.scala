package com.scalaAsm.x86

sealed trait OpcodeFormat {
  def size: Int
  def get: Array[Byte]
  val opcodeExtension: Option[Byte] // some opcodes use the ModRM field
  def /+(x: Byte): OpcodeFormat
  def isOpcodePlus: Boolean
  def prefix: Seq[Prefix]
}

sealed trait RegInOpcode
sealed trait RegInModRM

case object r extends RegInModRM

case object rw extends RegInOpcode
case object rd extends RegInOpcode
case object rb extends RegInOpcode
case object ro extends RegInOpcode

case class OneOpcode(operand: Byte, prefix: Seq[Prefix]) extends OpcodeFormat {
  def get = Array(operand)
  val size = 1
  val opcodeExtension: Option[Byte] = None
  def /+(x: Byte) = new OneOpcode(operand, prefix) { override val opcodeExtension = Some(x) }
  def /(x: RegInModRM) = this
  def +(reg: RegInOpcode) = new OpcodeWithReg(operand, prefix)
  def isOpcodePlus = false
}

class OpcodeWithReg(opcode: Byte, prefix: Seq[Prefix]) extends OneOpcode(opcode, prefix) {
    var reg: reg = _
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
  def /(x: RegInModRM) = this
  def +(reg: RegInOpcode) = new TwoOpcodeWithReg(opcode1, opcode2, prefix)
  def isOpcodePlus = false
}

class TwoOpcodeWithReg(opcode: Byte, opcode2: Byte, prefix: Seq[Prefix]) extends TwoOpcodes(opcode, opcode2, prefix) {
    var reg: reg = _
    override def get = Array((opcode + reg.ID).toByte)
    override val size = 1
    override val opcodeExtension: Option[Byte] = None
    override def /+(x: Byte) = new TwoOpcodes(opcode, opcode2, prefix) { override val opcodeExtension = Some(x) }
    override def isOpcodePlus = true
}

case class ThreeOpcodes(opcode1: Byte, opcode2: Byte, opcode3: Byte, prefix: Seq[Prefix]) extends OpcodeFormat {
  def get = Array(opcode1, opcode2, opcode3)
  val size = 3
  val opcodeExtension: Option[Byte] = None
  def /+(x: Byte) = new ThreeOpcodes(opcode1, opcode2, opcode3, prefix) { override val opcodeExtension = Some(x) }
  def /(x: RegInModRM) = this
  def isOpcodePlus = false
}

