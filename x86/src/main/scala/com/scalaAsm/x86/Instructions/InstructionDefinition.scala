package com.scalaAsm.x86
package Instructions

import Operands._
import Memory._
import com.scalaAsm.x86.OpcodeFormat

sealed trait x86Instruction {
  import scala.language.implicitConversions
  val mnemonic: String
  def opcode: OpcodeFormat
  def prefix = Seq[Prefix]()
  def opcodeSelectsRegister = opcode.isInstanceOf[OpcodeWithReg]
  def opcodeExtension: Byte = if (!opcode.opcodeExtension.isEmpty) opcode.opcodeExtension.get else -1
  def hasRMByte: Boolean = opcode.hasModRMByte || opcode.opcodeExtension.isDefined
  
  implicit def toPrefixSeq(x: Prefix) = Seq(x)
  implicit def toByte(x: Int) = x.toByte
  implicit def toOneOpcode(x: Int): OneOpcode = OneOpcode(x.toByte, prefix, false)
  implicit def toTwoOpcodes(x: (Int, Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte, prefix, false)
  implicit def toThreeOpcodes(x: (Int, Int, Int)): ThreeOpcodes = ThreeOpcodes(x._1.toByte, x._2.toByte, x._3.toByte, prefix, false)
}

trait InstructionDefinition {
  val mnemonic: String
  
  trait NoOp extends x86Instruction {
    val mnemonic = InstructionDefinition.this.mnemonic
    def get = ZeroMachineCode(new NoOperandFormat {}, opcode, mnemonic)
    def hasImplicitOperand: Boolean = false
  }

  trait OneOp[-O1] extends x86Instruction with OneOperandFormats[O1] {
    val mnemonic = InstructionDefinition.this.mnemonic
    def hasImplicitOperand: Boolean = false
    val format: OneOperandFormat[O1]
    
    def apply[X <: O1](p1: X, prefix: Seq[Prefix]) = {
      val prefixBytes = format.getPrefix(prefix, p1).map(_.get).foldLeft(Array[Byte]()) { _ ++ _ }
      OneMachineCode(opcode, p1, prefixBytes, mnemonic, format)
    }
  }

  trait TwoOp[-O1, -O2] extends x86Instruction with TwoOperandFormats[O1, O2]  {
    val mnemonic = InstructionDefinition.this.mnemonic
    def hasImplicitOperand: Boolean = false
    val format: TwoOperandFormat[O1, O2]

    def apply[X <: O1, Y <: O2](p1: X, p2: Y) = {
      val prefixBytes = format.getPrefix(prefix, p1, p2).map(_.get).foldLeft(Array[Byte]()) { _ ++ _ }
      TwoMachineCode(opcode, p1, p2, prefixBytes, mnemonic, format)
    }
  }
}