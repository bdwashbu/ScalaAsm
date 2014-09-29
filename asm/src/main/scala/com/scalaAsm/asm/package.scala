package com.scalaAsm

import scala.collection.mutable.ListBuffer
import com.scalaAsm.asm.Tokens._

package object asm {

  trait x86Mode
  trait x86_32 extends x86Mode
  trait x86_64 extends x86Mode
  
  trait AsmSection[X] {
    val builder = new ListBuffer[X]()
  }
  
}