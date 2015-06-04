package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86._
import com.scalaAsm.x86.Instructions._

case class AbsoluteAddress[Size: x86Size: ConstantWriter](offset: Size) extends Memory[Size] {
  val name: String = ""
  val getBytes: Array[Byte] = Constant(offset).getBytes
  override def size(next: ModRM) = implicitly[x86Size[Size]].size
}

case class BaseIndex[Size: x86Size](base: GeneralPurpose[Size], displacement: Constant[_]) extends Memory[Size] {
  override def toString = "[" + base.name + " " + (if (displacement.value.toString.contains('-')) '-' else '+') + "]"
  override def size(next: ModRM) = implicitly[x86Size[Size]].size
}

case class Indirect[Size: x86Size](base: GeneralPurpose[Size]) extends Memory[Size] {
  override def size(next: ModRM) = implicitly[x86Size[Size]].size
}

