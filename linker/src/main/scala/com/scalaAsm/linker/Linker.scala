package com.scalaAsm.linker

import com.scalaAsm.coff.Coff
import com.scalaAsm.portableExe.PortableExecutable

abstract class Linker {
  def link(objFile: Coff, addressOfData: Int, is64Bit: Boolean, dlls: String*): PortableExecutable
}