package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

trait AsmData {
  self: AsmProgram =>
    
  import Tokens._
    
  val data: Data

    
  class Data {
    
    protected val dataTokens = new ListBuffer[Token]()
	
    def compile: List[Token] = {
      dataTokens.toList.head +: dataTokens.toList.tail.flatMap{ token => List(token, align(0x4, 0x00))}
    }

    def align(to: Int, filler: Byte = 0xCC.toByte): Token = {
      Align(to, filler, (parserPos) => (to - (parserPos % to)) % to)
    }
  }
  
}