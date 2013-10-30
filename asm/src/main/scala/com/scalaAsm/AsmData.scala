package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer

trait AsmData {
  self: AsmProgram =>
    
  import Tokens._
    
  val data: Data
  protected val dataTokens = new ListBuffer[Token]()
    
  case class Data {
	
    def compile = {
      var isFirst = true
      data.getClass().getDeclaredFields().foreach { field =>
        field.setAccessible(true)
    	dataTokens += Variable(field.getName(), field.get(data).asInstanceOf[String], field.get(data).asInstanceOf[String].length)
    	if (!isFirst)
    	  align(0x4, 0x00)
    	else
    	  isFirst = false
      }
    }

    def align(to: Int, filler: Byte = 0xCC.toByte) = {
      dataTokens += Align(to, filler, (parserPos) => (to - (parserPos % to)) % to)
    }
  }
  
}