package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86._
import com.scalaAsm.x86.Instructions._

case class AbsoluteAddress[Size: x86Size: ConstantWriter](address: Constant[Size], name: String) extends Memory[Size] {
  val getBytes: Array[Byte] = address.getBytes
  override def size(next: Byte) = implicitly[x86Size[Size]].size
}

case class BaseIndex[Size: x86Size](base: GeneralPurpose[Size], displacement: Constant[_]) extends Memory[Size] {
  override def toString = "[" + base.name + " " + (if (displacement.value.toString.contains('-')) '-' else '+') + "]"
  override def size(next: Byte) = implicitly[x86Size[Size]].size
}

case class Indirect[Size: x86Size](base: GeneralPurpose[Size]) extends Memory[Size] {
  override def size(next: Byte) = implicitly[x86Size[Size]].size
}

