package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer

trait AsmData {
  self: AsmProgram =>
    
  import Tokens._
    
  val data: Data
  protected val dataTokens = new ListBuffer[Token]()
    
  case class Data {
	
    protected implicit class ImplicitVariable(val name: String) {
      def db(value: String) = {
        dataTokens += Variable(name, value, value.length)
      }
    }

    def apply(contents: => Unit) {
      contents
    }

    def align(to: Int, filler: Byte = 0xCC.toByte) = {
      dataTokens += Align(to, filler, (parserPos) => (to - (parserPos % to)) % to)
    }
  }
  
}