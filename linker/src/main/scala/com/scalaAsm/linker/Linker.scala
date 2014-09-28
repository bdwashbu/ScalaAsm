package com.scalaAsm.linker

import com.scalaAsm.portableExe.CompiledImports
import com.scalaAsm.portableExe.DosHeader
import com.scalaAsm.portableExe.PeHeader
import com.scalaAsm.portableExe.DataDirectories
import com.scalaAsm.portableExe.sections.Sections
import com.scalaAsm.portableExe.sections.ImageExportDirectory
import com.scalaAsm.portableExe.sections.Extern
import com.scalaAsm.portableExe.sections.Imports
import com.scalaAsm.portableExe.{PortableExecutable, PortableExecutable32}
import com.scalaAsm.portableExe.sections.SectionHeader
import com.scalaAsm.portableExe.sections.Characteristic
import com.scalaAsm.portableExe.OptionalHeader
import com.scalaAsm.portableExe.AdditionalFields
import com.scalaAsm.portableExe.ImageDataDirectory
import com.scalaAsm.portableExe.FileHeader
import com.scalaAsm.portableExe.NtHeader
import com.scalaAsm.portableExe.sections.ResourceGen
import com.scalaAsm.coff.Assembled

abstract class Linker {
  def link(assembled: Assembled, addressOfData: Int, is64Bit: Boolean, dlls: String*): PortableExecutable
}