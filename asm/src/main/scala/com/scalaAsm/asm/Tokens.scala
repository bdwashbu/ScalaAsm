package com.scalaAsm.asm

import com.scalaAsm.x86.MachineCode

object Tokens {
  trait Token
  
  case class Reference(name: String) extends CodeToken
  case class JmpRef(name: String) extends CodeToken

  case class Procedure(name: String, innerCode: List[CodeToken]) extends CodeToken
  case class Label(name: String) extends CodeToken
  case class LabelRef(labelRef: String, opCode: Byte) extends CodeToken

  trait CodeToken extends Token
  abstract class SizedToken(val size: Int) extends Token
  case class InstructionToken(inst: MachineCode) extends SizedToken(inst.size) with CodeToken
  case class BeginProc(name: String) extends SizedToken(0)
  case class Padding(to: Int, tokenSize: Int) extends SizedToken(tokenSize)
  case class ProcRef(procName: String) extends SizedToken(5)
  case class VarRef(procName: String) extends SizedToken(5)
  case class ImportRef(varName: String) extends SizedToken(6)
  case class JmpRefResolved(varName: String) extends SizedToken(6)
  case class Variable(name: String, val value: String, tokenSize: Int) extends SizedToken(tokenSize)

  abstract class DynamicSizedToken(val size: (Int) => Int) extends Token
  case class Align(to: Int, filler: Byte, override val size: (Int) => Int) extends DynamicSizedToken(size) with CodeToken

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