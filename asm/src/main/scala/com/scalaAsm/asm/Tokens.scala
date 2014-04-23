package com.scalaAsm.asm

import com.scalaAsm.x86.MachineCode
import com.scalaAsm.x86.Instruction
import com.scalaAsm.x86.Operands.Immediate8
import com.scalaAsm.x86.OneOperandInstruction

object Tokens {
  trait Token
  
  trait CodeToken extends Token
  case class Reference(name: String) extends CodeToken
  case class JmpRef(name: String) extends CodeToken
  case class CodeGroup(code: Seq[CodeToken]) extends CodeToken
  case class ProcedureToken(name: String, innerCode: Seq[CodeToken]) extends CodeToken
  case class Label(name: String) extends CodeToken
  case class LabelRef(labelRef: String, inst:OneOperandInstruction[Immediate8]) extends CodeToken
  case class InstructionToken(inst: MachineCode) extends SizedToken(inst.size) with CodeToken
  case class Align(to: Int, filler: Byte, override val size: (Int) => Int) extends DynamicSizedToken(size) with CodeToken
  
  abstract class SizedToken(val size: Int) extends Token
  case class BeginProc(name: String) extends Token
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