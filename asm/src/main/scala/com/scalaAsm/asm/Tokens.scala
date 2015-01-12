package com.scalaAsm.asm

import com.scalaAsm.x86.Instructions._
import com.scalaAsm.x86.Operands.Constant8
import com.scalaAsm.x86.Operands.NewOneOperandFormat
import com.scalaAsm.x86.OpcodeFormat
import com.scalaAsm.x86.Operands.Constant
import com.scalaAsm.x86.OneOpcode
import com.scalaAsm.x86.InstructionResult

object Tokens {
  trait Token
  
  trait CodeToken extends Token
  case class Reference(name: String) extends CodeToken with InstructionResult {
    def mnemonic = ""
    def getSize = 0
    def getBytes = Array()
  }
  case class Invoke(name: String) extends CodeToken with InstructionResult {
    def mnemonic = ""
    def getSize = 0
    def getBytes = Array()
  }
  case class JmpRef(name: String) extends CodeToken

  case class ProcedureToken(name: String, innerCode: Seq[InstructionResult]) extends CodeToken with InstructionResult {
    def mnemonic = ""
    def getSize = 0
    def getBytes = Array()
  }
  case class Label(name: String) extends CodeToken with InstructionResult {
    def mnemonic = ""
    def getSize = 0
    def getBytes = Array()
  }
  case class LabelRef(labelRef: String, inst:InstructionDefinition[OneOpcode]#_1_new[Constant8], format: NewOneOperandFormat[Constant8]) extends CodeToken with InstructionResult {
    def mnemonic = ""
    def getSize = 0
    def getBytes = Array()
  } 
  case class InstructionToken(inst: InstructionResult) extends SizedToken(inst.getSize) with CodeToken
  case class Align(to: Int, filler: Byte, override val size: (Int) => Int) extends DynamicSizedToken(size) with CodeToken with InstructionResult {
    def mnemonic = ""
    def getSize = 0
    def getBytes = Array()
  }
  
  abstract class SizedToken(val size: Int) extends Token
  case class BeginProc(name: String) extends Token with InstructionResult {
    def mnemonic = ""
    def getSize = 0
    def getBytes = Array()
  }
  case class Padding(to: Int, tokenSize: Int) extends SizedToken(tokenSize)
  case class ProcRef(procName: String) extends SizedToken(5)
  case class VarRef(procName: String) extends SizedToken(5)
  
  case class JmpRefResolved(varName: String) extends SizedToken(6)
  
  trait DataToken extends Token
  case class Variable(name: String, val value: String) extends SizedToken(value.length) with DataToken

  abstract class DynamicSizedToken(val size: (Int) => Int) extends Token

  sealed trait PostToken
  case class ImportRef(position: Int, varName: String) extends SizedToken(5) with PostToken
  case class InvokeRef(position: Int, varName: String) extends SizedToken(5) with PostToken
  case class LabelResolved(position: Int, name: String) extends PostToken
  case class LabelRefResolved(position: Int, name: String, opCode: Byte) extends PostToken
  case class ByteOutputPost(bytes: Array[Byte]) extends PostToken
  case class Proc(pos: Int, name: String) extends PostToken
  case class PostVar(name: String, value: String, val position: Int) extends PostToken
  case class ProcState(pos: Int, name: String) extends PostToken
  case class VarState(name: String) extends PostToken
  case class JmpState(name: String) extends PostToken
  case class ImportState(name: String) extends PostToken
}