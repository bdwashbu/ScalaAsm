package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86._
import com.scalaAsm.x86.Instructions._

sealed trait Memory[Size] extends RegisterOrMemory[Size] 

case class AbsoluteAddress[Size: x86Size: ConstantWriter](offset: Size) extends Memory[Size] {
  val name: String = ""
  val getBytes: Array[Byte] = Constant(offset).getBytes
}

case class BaseIndex[Y: x86Size](base: GeneralPurpose[Y], displacement: Constant[_]) extends Memory[Y] {
    def get: BaseIndex[Y] = this
    override def toString = "[" + base.name + " " + (if (displacement.value.toString.contains('-')) '-' else '+') + "]"
  }

case class Indirect[S: x86Size](base: GeneralPurpose[S]) extends Memory[S]

