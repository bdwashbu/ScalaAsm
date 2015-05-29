package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86._

trait Memory[Size] extends RegisterOrMemory[Size] 

case class AbsoluteAddress[Size: x86Size: ConstantWriter](offset: Size) extends Memory[Size] {
  val name: String = ""
  val getBytes: Array[Byte] = Constant(offset).getBytes
}

