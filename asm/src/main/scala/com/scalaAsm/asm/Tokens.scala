package com.scalaAsm.asm

import com.scalaAsm.x86.Instructions._
import com.scalaAsm.x86.Operands.OneOperandFormat
import com.scalaAsm.x86.OpcodeFormat
import com.scalaAsm.x86.Operands.Constant
import com.scalaAsm.x86._
import com.scalaAsm.x86.OneOpcode
import com.scalaAsm.x86.InstructionResult

object Tokens {
  trait Token
  
  trait CodeToken extends Token
  case class Reference(name: String) extends CodeToken with InstructionResult {
    def mnemonic = ""
    def apply = Array()
  }
  
  case class FunctionReference(name: String) extends CodeToken with InstructionResult {
    def mnemonic = ""
    def apply = Array()
  }
  
  case class Address(name: String) extends CodeToken with InstructionResult {
    def mnemonic = ""
    def apply = Array()
  }
  
  case class Invoke(name: String) extends CodeToken with InstructionResult {
    def mnemonic = ""
    def apply = Array()
  }

  case class ProcedureToken(name: String, innerCode: Seq[InstructionResult]) extends CodeToken with HighLevel
  case class Label(name: String) extends CodeToken with InstructionResult {
    def mnemonic = ""  
    def apply = Array()
  }
  case class LabelRef(labelRef: String, inst:InstructionDefinition#_1[Constant[_8]], format: OneOperandFormat[Constant[_8]]) extends CodeToken with InstructionResult {
    def mnemonic = ""
    
    def apply = Array()
  } 
  case class InstructionToken(inst: InstructionResult) extends SizedToken(inst.getSize) with CodeToken
  case class Align(to: Int, filler: Byte, override val size: (Int) => Int) extends DynamicSizedToken(size) with CodeToken with HighLevel with InstructionResult {
    def mnemonic = ""
    def apply = Array()
  }
  
  abstract class SizedToken(val size: Int) extends Token
  case class BeginProc(name: String) extends Token with InstructionResult {
    def mnemonic = ""
    def apply = Array()
  }
  case class Padding(to: Int, tokenSize: Int) extends SizedToken(tokenSize)
  case class ProcRef(procName: String) extends SizedToken(5) with InstructionResult {
    def mnemonic = ""
    def apply = Array()
  }
  case class VarRef(procName: String) extends SizedToken(5)
  

  trait DataToken extends Token
  case class Variable(name: String, val value: String) extends SizedToken(value.length) with DataToken

  abstract class DynamicSizedToken(val size: (Int) => Int) extends Token

  sealed trait PostToken
  case class ImportRef(position: Int, varName: String) extends SizedToken(5) with PostToken with InstructionResult {
    def mnemonic = ""
    def apply = Array()
  }
  case class InvokeRef(position: Int, varName: String) extends SizedToken(5) with PostToken
  case class LabelResolved(position: Int, name: String) extends PostToken
  case class JmpResolved(position: Int, name: String) extends PostToken
  case class LabelRefResolved(position: Int, name: String, opCode: Byte) extends PostToken
  case class ByteOutputPost(bytes: Array[Byte]) extends PostToken
  case class Proc(pos: Int, name: String) extends PostToken
  case class PostVar(name: String, value: String, val position: Int) extends PostToken
  case class ProcState(pos: Int, name: String) extends PostToken
  case class VarState(name: String) extends PostToken
  //case class JmpState(name: String) extends PostToken
  case class ImportState(name: String) extends PostToken
}