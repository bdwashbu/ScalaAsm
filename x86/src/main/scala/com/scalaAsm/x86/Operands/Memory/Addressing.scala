package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86._

trait AddressingMode[Size] extends RegisterOrMemory[Size] 

trait AbsoluteAddress[Size] extends AddressingMode[Size] with Operand[AbsoluteAddress[Size]] {
  self =>
  var offset: Size
  def getRelative: Relative[Size]
  def displacement: Constant[Size]
  def get = this
}

abstract class Relative[S: x86Size] extends RegisterOrMemory[S] with Operand[Relative[S]]{
  self =>
    def displacement: Constant[S]
    def get = this
}