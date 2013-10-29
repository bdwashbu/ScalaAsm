package com.scalaAsm.asm

object Tokens {
	sealed trait Token
	case class Reference(val name: String) extends Token
	case class JmpRef(val name: String) extends Token
	
	case class Label(val name: String) extends Token
	case class LabelRef(val labelRef: String, val opCode: Byte) extends Token
	
	abstract class SizedToken(val size: Int) extends Token
	case class CodeToken(val bytes: Array[Byte]) extends SizedToken(bytes.size)
	case class BeginProc(val name: String) extends SizedToken(0)
	case class Padding(val to: Int, tokenSize: Int) extends SizedToken(tokenSize)
	case class ProcRef(val procName: String) extends SizedToken(5)
	case class VarRef(val procName: String) extends SizedToken(5)
	case class ImportRef(val varName: String) extends SizedToken(6)
	case class JmpRefResolved(val varName: String) extends SizedToken(6)
	case class Variable(val name: String, val value: String, tokenSize: Int) extends SizedToken(tokenSize)
	
	abstract class DynamicSizedToken(val size: (Int) => Int) extends Token
	case class Align(val to: Int, val filler: Byte, override val size: (Int) => Int) extends DynamicSizedToken(size)
	
	sealed trait PostToken
	case class LabelResolved(val position: Int, val name: String) extends PostToken
	case class LabelRefResolved(val position: Int, val name: String, val opCode: Byte) extends PostToken
    case class ByteOutputPost(val bytes: Array[Byte]) extends PostToken
    case class Proc(val pos: Int, val name: String) extends PostToken
    case class PostVar(val name: String, val value: String, val position: Int) extends PostToken
    case class ProcState(val pos: Int, val name: String) extends PostToken
    case class VarState(val pos: Int, val name: String) extends PostToken
    case class JmpState(val pos: Int, val name: String) extends PostToken
    case class ImportState(val pos: Int, val name: String) extends PostToken
}