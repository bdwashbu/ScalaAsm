package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

abstract class x86Instruction(mnemonic: String) extends Instruction {
  implicit def toByte(x: Int) = x.toByte
  implicit def toOneOpcode(x: Int): OneOpcode = OneOpcode(x.toByte)
  implicit def toTwoOpcodes(x: (Int, Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte)
}

trait InstructionField extends Any {
  def getBytes: Array[Byte]
}

trait Instruction {
  
  case class OpcodePlusRd(opcode1: Byte) extends OpcodeFormat {
    def get(x: Any) = {
      if (x.isInstanceOf[ModRM.reg])
        Array((opcode1 + x.asInstanceOf[ModRM.reg].ID).toByte)
      else
        Array()

    }
    val size = 1
    val opcodeExtension: Option[Byte] = None
    def /+(x: Byte) = new OneOpcode(opcode1) { override val opcodeExtension = Some(x) }
  }

  case class OneOpcode(operand1: Byte) extends OpcodeFormat {
    def get(x: Any) = Array(operand1)
    val size = 1
    val opcodeExtension: Option[Byte] = None
    def /+(x: Byte) = new OneOpcode(operand1) { override val opcodeExtension = Some(x) }
  }

  case class TwoOpcodes(opcode1: Byte, opcode2: Byte) extends OpcodeFormat {
    def get(x: Any) = Array(opcode1, opcode2)
    val size = 2
    val opcodeExtension: Option[Byte] = None
    def /+(x: Byte) = new TwoOpcodes(opcode1, opcode2) { override val opcodeExtension = Some(x) }
  }
}

trait ZeroOperandInstruction extends Instruction {
  def opcode: OpcodeFormat
  val mnemonic: String = ""

  def apply: MachineCode =
    new MachineCode {
      val size = getSize
      val code = getBytes
      val line = mnemonic
    }

  def getSize: Int = {
    opcode.size
  }

  def getBytes: Array[Byte] = {
    opcode.get(null)
  }
}

trait OneOperandInstruction[-O1] extends Instruction with OneOperandInstructionFormat {
  val opcode: OpcodeFormat
  def operands: OneOperandFormat[O1]
  val mnemonic: String = ""

  def apply(x: O1): MachineCode =
    new MachineCode {
      val size = getSize(x)
      val code = getBytes(x)
      val line = mnemonic + " " + operands.toString
    }

  def getSize(x: O1): Int = {
    opcode.size + (operands.getAddressingForm(x, opcode) match {
      case Some(modRM) => modRM.size
      case _ => 0
    })
  }

  def getBytes(x: O1): Array[Byte] = {
    opcode.get(x) ++ (operands.getAddressingForm(x, opcode) match {
      case Some(modRM) => modRM.getBytes
      case _ => Array.emptyByteArray
    })
  }
}

trait TwoOperandInstruction[-O1, -O2] extends Instruction with TwoOperandInstructionFormat {
  val opcode: OpcodeFormat
  def operands: TwoOperandsFormat[O1, O2]
  val mnemonic: String = ""

  def apply(x: O1, y: O2): MachineCode =
    new MachineCode {
      val size = getSize(x, y)
      val code = getBytes(x, y)
      val line = mnemonic + " " + operands.toString
    }

  def getSize(x: O1, y: O2): Int = {
    opcode.size + (operands.getAddressingForm(x, y, opcode) match {
      case Some(modRM) => modRM.size
      case _ => 0
    })
  }

  def getBytes(x: O1, y: O2): Array[Byte] = {
    opcode.get(x) ++ (operands.getAddressingForm(x, y, opcode) match {
      case Some(modRM) => modRM.getBytes
      case _ => Array.emptyByteArray
    })
  }
}
