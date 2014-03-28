package com.scalaAsm

import scala.collection.mutable.ListBuffer
import com.scalaAsm.asm.Tokens._

package object asm {

  trait AsmSegment {
    val builder = new ListBuffer[Token]()
  }
  
}