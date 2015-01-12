package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86._

trait Memory[Size] extends RegisterOrMemory[Size] 

trait AbsoluteAddress[Size] extends Memory[Size] with Operand[AbsoluteAddress[Size]] {
  self =>
  var offset: Size
  def getRelative: Constant[Size]
  def displacement: Constant[Size]
  def get = this
}