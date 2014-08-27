package com.scalaAsm.asm

import com.scalaAsm.x86.InstructionResult
import com.scalaAsm.x86.Instructions._
import com.scalaAsm.x86.Operands.Constant8
import com.scalaAsm.x86.Operands.OneOperandFormat
import com.scalaAsm.x86.OpcodeFormat

object Tokens {
  trait Token
  
  trait CodeToken extends Token
  case class Reference(name: String) extends CodeToken with InstructionResult {
    def line = ""
    def getSize = 0
    def getBytes = Array()
  }
  case class JmpRef(name: String) extends CodeToken
  case class CodeGroup(code: Seq[InstructionResult]) extends CodeToken with InstructionResult {
    def line = ""
    def getSize = 0
    def getBytes = Array()
  }
  case class ProcedureToken(name: String, innerCode: Seq[InstructionResult]) extends CodeToken with InstructionResult {
    def line = ""
    def getSize = 0
    def getBytes = Array()
  }
  case class Label(name: String) extends CodeToken with InstructionResult {
    def line = ""
    def getSize = 0
    def getBytes = Array()
  }
  case class LabelRef[OpEn](labelRef: String, inst:OneOperandInstruction[OpEn, Constant8], format: OneOperandFormat[OpEn, Constant8]) extends CodeToken with InstructionResult {
    def line = ""
    def getSize = 0
    def getBytes = Array()
  } 
  case class InstructionToken(inst: InstructionResult) extends SizedToken(inst.getSize) with CodeToken
  case class Align(to: Int, filler: Byte, override val size: (Int) => Int) extends DynamicSizedToken(size) with CodeToken with InstructionResult {
    def line = ""
    def getSize = 0
    def getBytes = Array()
  }
  
  abstract class SizedToken(val size: Int) extends Token
  case class BeginProc(name: String) extends Token with InstructionResult {
    def line = ""
    def getSize = 0
    def getBytes = Array()
  }
  case class Padding(to: Int, tokenSize: Int) extends SizedToken(tokenSize)
  case class ProcRef(procName: String) extends SizedToken(5)
  case class VarRef(procName: String) extends SizedToken(5)
  case class ImportRef(varName: String) extends SizedToken(6)
  case class JmpRefResolved(varName: String) extends SizedToken(6)
  
  trait DataToken extends Token
  case class Variable(name: String, val value: String) extends SizedToken(value.length) with DataToken

  abstract class DynamicSizedToken(val size: (Int) => Int) extends Token

  sealed trait PostToken
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