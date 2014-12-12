package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import com.scalaAsm.asm.Tokens.Token

abstract class DataSection(tokens: Token*) extends AsmSection {
  
  import Tokens._

  def compile: Seq[Token] = {
    tokens.toSeq.head +: tokens.toSeq.tail.flatMap { token => Seq[Token](token, align(0x4, 0x00)) }
  }

  def align(to: Int, filler: Byte = 0xCC.toByte): Token = {
    Align(to, filler, (parserPos) => (to - (parserPos % to)) % to)
  }
}