package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

abstract class x86Instruction(mnemonic: String) extends Instruction {
  implicit def toByte(x: Int) = x.toByte
  implicit def toOneOpcode(x: Int): OneOpcode = OneOpcode(x.toByte)
  implicit def toTwoOpcodes(x: (Int, Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte)
}

trait Instruction

trait ZeroOperandInstruction extends Instruction {
  def opcode: OpcodeFormat
  val mnemonic: String = ""

  def apply: MachineCodeBuilder =
    new MachineCodeBuilder {
      def get = new MachineCode {
          val size = getSize
          val code = getBytes
          val line = mnemonic
      }
    }

  def getSize: Int = {
    opcode.size
  }

  def getBytes: Array[Byte] = {
    opcode.get(null)
  }
}

trait OneOperandInstruction[-O1 <: Operand] extends Instruction {
  val opcode: OpcodeFormat
  def opEn: OneOperandFormat[O1]
  val mnemonic: String = ""

  def apply(x: O1) =
    new MachineCodeBuilder1[O1](x) {
      def get = new MachineCode {
        val size = getSize(x)
        val code = getBytes(x)
        val line = mnemonic + " " + opEn.toString
      }
    }

  def getSize(x: O1): Int = {
    opcode.size + opEn.getAddressingForm(x, opcode).size
  }

  def getBytes(x: O1): Array[Byte] = {
    opcode.get(x) ++ opEn.getAddressingForm(x, opcode).getBytes
  }
}

trait TwoOperandInstruction[-O1 <: Operand, -O2 <: Operand] extends Instruction {
  val opcode: OpcodeFormat
  def opEn: TwoOperandsFormat[O1, O2]
  val mnemonic: String = ""
    
  def apply(x: O1, y: O2) =
    new MachineCodeBuilder2[O1, O2](x, y) {
      def get = new MachineCode {
        val size = getSize(x, y)
        val code = getBytes(x, y)
        val line = mnemonic + " " + opEn.toString
      }
    }

  def getSize(x: O1, y: O2): Int = {
    val prefixes = opEn.getPrefixes(x, y) getOrElse Array()
    prefixes.size + opcode.size + opEn.getAddressingForm(x, y, opcode).size
  }

  def getBytes(x: O1, y: O2): Array[Byte] = {
    val prefixes = opEn.getPrefixes(x, y) getOrElse Array()
    prefixes ++ opcode.get(x) ++ opEn.getAddressingForm(x, y, opcode).getBytes
  }
}
