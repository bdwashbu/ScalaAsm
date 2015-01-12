package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands.TwoOperandFormat
import com.scalaAsm.x86.Operands.OneOperandFormat
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.NoOperandFormat
import com.scalaAsm.x86.Operands.Constant
import com.scalaAsm.x86.OpcodeWithReg
import com.scalaAsm.x86.Operands.Memory.ModRM
import Memory._

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

case class OneMachineCode[O1, OpEn](
    operand: Operand[O1],
    instBytes: Array[Byte],
    prefixAndOpcode: Array[Byte],
    mnemonic: String,
    opcodeExtension: Byte) extends InstructionResult {
  
  override def toString = {
    val formattedMnemonic = mnemonic.head + mnemonic.tail.toLowerCase()
    formattedMnemonic + " " + operand.toString
  }

  def getSize: Int = {
    prefixAndOpcode.size + instBytes.size
  }

  def getBytes: Array[Byte] = {
      prefixAndOpcode ++: instBytes
  }
}

case class TwoMachineCode[O1, O2, OpEn](
    operand: Operand[O1],
    operand2: Operand[O2],
    instBytes: Array[Byte],
    prefixAndOpcode: Array[Byte],
    mnemonic: String) extends InstructionResult {
  
  override def toString = {
    val formattedMnemonic = mnemonic.head + mnemonic.tail.toLowerCase()
    formattedMnemonic + " " + operand.toString + ", " + operand2.toString
  }

  def getSize: Int = {
    prefixAndOpcode.size + instBytes.size
  }

  def getBytes: Array[Byte] = {
    prefixAndOpcode ++: instBytes
  }
}

abstract class InstructionDefinition[Opcode <: OpcodeFormat](val mnemonic: String) {

  abstract class _0 extends x86Instruction {
    def opcode: Opcode
    val mnemonic = InstructionDefinition.this.mnemonic
    def get = ZeroMachineCode(new NoOperandFormat {}, opcode, mnemonic)
  }
  
  abstract class _0_new extends x86Instruction {
    def opcode: Opcode
    val mnemonic = InstructionDefinition.this.mnemonic
    def get = ZeroMachineCode(new NoOperandFormat {}, opcode, mnemonic)
    def hasImplicateOperand: Boolean = false
  }
  
  abstract class _1[-O1, OpEn]  extends x86Instruction {
    val mnemonic = InstructionDefinition.this.mnemonic
    def opcode: Opcode
    
    def apply[X <: O1](p1: Operand[X], format: OneOperandFormat[X, OpEn], prefix: Seq[Prefix]) = {  
      val opEx = if (!opcode.opcodeExtension.isEmpty) opcode.opcodeExtension.get else 0
      
      if (opcode.isInstanceOf[OpcodeWithReg] && p1.get.isInstanceOf[reg]) { // this is hacky as hell!
        val opcodePlus = opcode.asInstanceOf[OpcodeWithReg]
        opcodePlus.reg = p1.get.asInstanceOf[reg] 
        val opcodeBytes = format.getPrefix(prefix).map(_.get).foldLeft(Array[Byte]()){ _ ++ _ } ++: opcodePlus.get
        OneMachineCode(p1, format.getAddressingForm(p1.get, opEx).getBytes, opcodeBytes, mnemonic, opEx)
      } else {
        val opcodeBytes = format.getPrefix(prefix).map(_.get).foldLeft(Array[Byte]()){ _ ++ _ } ++: opcode.get
        OneMachineCode(p1, format.getAddressingForm(p1.get, opEx).getBytes, opcodeBytes, mnemonic, opEx)
      }
    }
  }
  
  abstract class _1_new[-O1]  extends x86Instruction {
    val mnemonic = InstructionDefinition.this.mnemonic
    def opcode: Opcode
    def hasImplicateOperand: Boolean = false
    def explicitFormat: Option[InstructionFormat] = None
    
    def apply[X <: O1](p1: Operand[X], implicitFormat: NewOneOperandFormat[X], prefix: Seq[Prefix]) = {  
      val opEx = if (!opcode.opcodeExtension.isEmpty) opcode.opcodeExtension.get else 0
      
      val addressForm = explicitFormat getOrElse implicitFormat.getAddressingForm(p1.get, opEx)
      
      if (opcode.isInstanceOf[OpcodeWithReg] && p1.get.isInstanceOf[reg]) { // this is hacky as hell!
        val opcodePlus = opcode.asInstanceOf[OpcodeWithReg]
        opcodePlus.reg = p1.get.asInstanceOf[reg] 
        val opcodeBytes = implicitFormat.getPrefix(prefix).map(_.get).foldLeft(Array[Byte]()){ _ ++ _ } ++: opcodePlus.get
        OneMachineCode(p1, addressForm.getBytes, opcodeBytes, mnemonic, opEx)
      } else {
        val opcodeBytes = implicitFormat.getPrefix(prefix).map(_.get).foldLeft(Array[Byte]()){ _ ++ _ } ++: opcode.get
        OneMachineCode(p1, addressForm.getBytes, opcodeBytes, mnemonic, opEx)
      }
    }
  }
  
  abstract class _2[-O1, -O2, OpEn]  extends x86Instruction {
    val mnemonic = InstructionDefinition.this.mnemonic
    def opcode: Opcode
    
    def apply[X <: O1, Y <: O2](p1: Operand[X], p2: Operand[Y], format: TwoOperandFormat[X, Y, OpEn], prefix: Seq[Prefix]) = {
      val opEx = if (!opcode.opcodeExtension.isEmpty) opcode.opcodeExtension.get else 0
      
      if (opcode.isInstanceOf[OpcodeWithReg] && p1.get.isInstanceOf[reg]) { // this is hacky as hell!
        val opcodePlus = opcode.asInstanceOf[OpcodeWithReg]
        opcodePlus.reg = p1.get.asInstanceOf[reg]
        val opcodeBytes = format.getPrefix(prefix) ++: opcodePlus.get
        TwoMachineCode(p1, p2, format.getAddressingForm(p1.get, p2.get, opEx).getBytes, opcodeBytes, mnemonic)
      } else {
        val opcodeBytes = format.getPrefix(prefix) ++: opcode.get
        TwoMachineCode(p1, p2, format.getAddressingForm(p1.get, p2.get, opEx).getBytes, opcodeBytes, mnemonic)
      }
    }
  }
  
   abstract class _2_new[-O1, -O2]  extends x86Instruction {
    val mnemonic = InstructionDefinition.this.mnemonic
    def opcode: Opcode
    def hasImplicateOperand: Boolean = false
    
    def apply[X <: O1, Y <: O2](p1: Operand[X], p2: Operand[Y], format: NewTwoOperandFormat[X, Y]) = {
      val opEx = if (!opcode.opcodeExtension.isEmpty) opcode.opcodeExtension.get else 0
      
      if (opcode.isInstanceOf[OpcodeWithReg] && p1.get.isInstanceOf[reg]) { // this is hacky as hell!
        val opcodePlus = opcode.asInstanceOf[OpcodeWithReg]
        opcodePlus.reg = p1.get.asInstanceOf[reg]
        val opcodeBytes = prefix.map(_.get).foldLeft(Array[Byte]()){ _ ++ _ } ++: opcodePlus.get
        TwoMachineCode(p1, p2, format.getAddressingForm(p1.get, p2.get, opEx).getBytes, opcodeBytes, mnemonic)
      } else {
        val opcodeBytes = prefix.map(_.get).foldLeft(Array[Byte]()){ _ ++ _ } ++: opcode.get
        TwoMachineCode(p1, p2, format.getAddressingForm(p1.get, p2.get, opEx).getBytes, opcodeBytes, mnemonic)
      }
    }
  }
}
