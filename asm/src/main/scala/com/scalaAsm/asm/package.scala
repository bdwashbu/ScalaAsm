package com.scalaAsm

import scala.collection.mutable.ListBuffer
import com.scalaAsm.asm.Tokens._

package object asm {

  trait AsmSection[X] {
    val builder = new ListBuffer[X]()
  }
  
}