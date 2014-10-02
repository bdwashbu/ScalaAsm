package com.scalaAsm.linker

import com.scalaAsm.coff.Assembled
import com.scalaAsm.portableExe.PortableExecutable

abstract class Linker {
  def link(assembled: Assembled, addressOfData: Int, is64Bit: Boolean, dlls: String*): PortableExecutable
}