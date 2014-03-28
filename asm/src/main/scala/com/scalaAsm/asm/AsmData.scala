package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

trait DataSegment extends AsmSegment {

  import Tokens._

  def compile: List[Token] = {
    builder.toList.head +: builder.toList.tail.flatMap { token => List(token, align(0x4, 0x00)) }
  }

  def align(to: Int, filler: Byte = 0xCC.toByte): Token = {
    Align(to, filler, (parserPos) => (to - (parserPos % to)) % to)
  }
}