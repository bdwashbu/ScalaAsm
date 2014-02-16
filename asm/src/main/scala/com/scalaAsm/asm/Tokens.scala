package com.scalaAsm.asm

object Tokens {
	sealed trait Token
	case class Reference(name: String) extends Token
	case class JmpRef(name: String) extends Token
	
	case class Label(name: String) extends Token
	case class LabelRef(labelRef: String, opCode: Byte) extends Token
	
	abstract class SizedToken(val size: Int) extends Token
	case class CodeToken(bytes: Array[Byte]) extends SizedToken(bytes.size)
	case class BeginProc(name: String) extends SizedToken(0)
	case class Padding(to: Int, tokenSize: Int) extends SizedToken(tokenSize)
	case class ProcRef(procName: String) extends SizedToken(5)
	case class VarRef(procName: String) extends SizedToken(5)
	case class ImportRef(varName: String) extends SizedToken(6)
	case class JmpRefResolved(varName: String) extends SizedToken(6)
	case class Variable(name: String, val value: String, tokenSize: Int) extends SizedToken(tokenSize)
	
	abstract class DynamicSizedToken(val size: (Int) => Int) extends Token
	case class Align(to: Int, filler: Byte, override val size: (Int) => Int) extends DynamicSizedToken(size)
	
	sealed trait PostToken
	case class LabelResolved(position: Int, name: String) extends PostToken
	case class LabelRefResolved(position: Int, name: String, opCode: Byte) extends PostToken
    case class ByteOutputPost(bytes: Array[Byte]) extends PostToken
    case class Proc(pos: Int, name: String) extends PostToken
    case class PostVar(name: String, value: String, val position: Int) extends PostToken
    case class ProcState(pos: Int, name: String) extends PostToken
    case class VarState(pos: Int, name: String) extends PostToken
    case class JmpState(pos: Int, name: String) extends PostToken
    case class ImportState(pos: Int, name: String) extends PostToken
}