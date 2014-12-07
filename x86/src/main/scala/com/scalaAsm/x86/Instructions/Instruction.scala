package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands.TwoOperandFormat
import com.scalaAsm.x86.Operands.OneOperandFormat
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.NoOperandFormat
import com.scalaAsm.x86.Operands.Constant
import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.OpcodePlus
import com.scalaAsm.x86.Operands.Memory.ModRM

trait InstructionField {
  def getBytes: Array[Byte]
  def size: Int
}

trait x86Instruction {
  import scala.language.implicitConversions
  val mnemonic: String
  def prefix = Seq[Prefix]()
  
  implicit def toPrefixSeq(x: Prefix) = Seq(x)
  implicit def toByte(x: Int) = x.toByte
  implicit def toOneOpcode(x: Int): OneOpcode = OneOpcode(x.toByte, prefix)
  implicit def toTwoOpcodes(x: (Int, Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte, prefix)
}

case class ZeroMachineCode(format: ResolvedZeroOperand, opcode: OpcodeFormat, mnemonic: String) extends InstructionResult {

  def getSize: Int = {
    format.getPrefix.size + opcode.size
  }

  def getBytes: Array[Byte] = {
    format.getPrefix ++: opcode.get
  }
}

case class OneMachineCode[O1, OpEn <: OneOperandEncoding[O1]](
    operand: Operand[O1],
    format: OneOperandFormat[O1, OpEn],
    prefixAndOpcode: Array[Byte],
    mnemonic: String,
    opcodeExtension: Byte) extends InstructionResult {
  
  override def toString = {
    val formattedMnemonic = mnemonic(0).toUpper.toString + mnemonic(1).toLower.toString + mnemonic(2).toLower.toString
    formattedMnemonic + " " + operand.toString
  }

  def getSize: Int = {
    prefixAndOpcode.size + format.size
  }

  def getBytes: Array[Byte] = {
      prefixAndOpcode ++: format.getAddressingForm(operand.get, opcodeExtension).getBytes
  }
}

case class TwoMachineCode[O1, O2, OpEn <: TwoOperandEncoding[O1,O2]](
    operand: Operand[O1],
    operand2: Operand[O2],
    format: TwoOperandFormat[O1, O2, OpEn],
    prefixAndOpcode: Array[Byte],
    mnemonic: String,
    opcodeExtension: Byte) extends InstructionResult {
  
  override def toString = {
    val formattedMnemonic = mnemonic(0).toUpper.toString + mnemonic(1).toLower.toString + mnemonic(2).toLower.toString
    formattedMnemonic + " " + operand.toString + ", " + operand2.toString
  }

  def getSize: Int = {
    prefixAndOpcode.size + format.size
  }

  def getBytes: Array[Byte] = {
    prefixAndOpcode ++: format.getAddressingForm(operand.get, operand2.get, opcodeExtension).getBytes
  }
}

abstract class InstructionDefinition[Opcode <: OpcodeFormat](val mnemonic: String) {

  abstract class _0 extends x86Instruction {
    def opcode: Opcode
    val mnemonic = InstructionDefinition.this.mnemonic
    def get = ZeroMachineCode(new NoOperandFormat {}, opcode, mnemonic)
  }
  
  abstract class _1[-O1, OpEn <: OneOperandEncoding[O1]]  extends x86Instruction {
    val mnemonic = InstructionDefinition.this.mnemonic
    def opcode: Opcode
    
    def apply[X <: O1](p1: Operand[X], format: OneOperandFormat[X, OpEn], prefix: Seq[Prefix]) = {  
      val opEx = if (!opcode.opcodeExtension.isEmpty) opcode.opcodeExtension.get else 0
      
      if (opcode.isInstanceOf[OpcodePlus] && p1.get.isInstanceOf[ModRM.reg]) { // this is hacky as hell!
        val opcodePlus = opcode.asInstanceOf[OpcodePlus]
        opcodePlus.reg = p1.get.asInstanceOf[ModRM.reg] 
        val opcodeBytes = format.getPrefix(prefix).map(_.get).foldLeft(Array[Byte]()){ _ ++ _ } ++: opcodePlus.get
        OneMachineCode(p1, format, opcodeBytes, mnemonic, opEx)
      } else {
        val opcodeBytes = format.getPrefix(prefix).map(_.get).foldLeft(Array[Byte]()){ _ ++ _ } ++: opcode.get
        OneMachineCode(p1, format, opcodeBytes, mnemonic, opEx)
      }
    }
  }
  
  abstract class _2[-O1, -O2, OpEn <: TwoOperandEncoding[O1, O2]]  extends x86Instruction {
    val mnemonic = InstructionDefinition.this.mnemonic
    def opcode: Opcode
    
    def apply[X <: O1, Y <: O2](p1: Operand[X], p2: Operand[Y], format: TwoOperandFormat[X, Y, OpEn], prefix: Seq[Prefix]) = {
      val opEx = if (!opcode.opcodeExtension.isEmpty) opcode.opcodeExtension.get else 0
      
      if (opcode.isInstanceOf[OpcodePlus] && p1.get.isInstanceOf[ModRM.reg]) { // this is hacky as hell!
        val opcodePlus = opcode.asInstanceOf[OpcodePlus]
        opcodePlus.reg = p1.get.asInstanceOf[ModRM.reg]
        val opcodeBytes = format.getPrefix(prefix).map(_.get).foldLeft(Array[Byte]()){ _ ++ _ } ++: opcodePlus.get
        TwoMachineCode(p1, p2, format, opcodeBytes, mnemonic, opEx)
      } else {
        val opcodeBytes = format.getPrefix(prefix).map(_.get).foldLeft(Array[Byte]()){ _ ++ _ } ++: opcode.get
        TwoMachineCode(p1, p2, format, opcodeBytes, mnemonic, opEx)
      }
    }
  }
}
