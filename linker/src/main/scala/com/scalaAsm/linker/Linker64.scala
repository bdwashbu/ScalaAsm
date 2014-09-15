package com.scalaAsm.linker

import com.scalaAsm.portableExe.CompiledImports
import com.scalaAsm.portableExe.DosHeader
import com.scalaAsm.portableExe.PeHeader
import com.scalaAsm.portableExe.DataDirectories
import com.scalaAsm.portableExe.sections._
import com.scalaAsm.portableExe.{PortableExecutable, PortableExecutable64}
import com.scalaAsm.portableExe.OptionalHeader
import com.scalaAsm.portableExe.AdditionalFields
import com.scalaAsm.portableExe.ImageDataDirectory
import com.scalaAsm.portableExe.FileHeader
import com.scalaAsm.portableExe.NtHeader
import com.scalaAsm.portableExe.sections.ResourceGen

class Linker64 extends Linker {
  
  def link(executableImports: CompiledImports, code: Array[Byte],
    addressOfData: Int, rawData: Array[Byte], resources: Option[Array[Byte]], is64Bit: Boolean): PortableExecutable = {

    val rawDataSize = rawData.length

    val dosHeader = DosHeader(
      e_magic = "MZ",
      e_cblp = 108,
      e_cp = 1,
      e_crlc = 0,
      e_cparhdr = 2,
      e_minalloc = 0,
      e_maxalloc = 65535.toShort,
      e_ss = 0,
      e_sp = 0,
      e_csum = 0,
      e_ip = 17,
      e_cs = 0,
      e_lfarlc = 64,
      e_ovno = 0,
      e_res = (0, 0, 26967, 13934),
      e_oemid = 8244,
      e_oeminfo = 29264,
      e_res2 = (26479, 24946, 8557, 2573, 46116.toShort, 47625.toShort, 256, 8653, 19636, 8653),
      e_lfanew = 96,
      watermark = "GoLink, GoAsm www.GoDevTool.com\0")

    val codeSection = Section(
      SectionHeader(
        name = "code",
        virtualSize = code.length,
        virtualAddress = 0x1000,
        sizeOfRawData = 0x200,
        pointerToRawData = 0x400,
        relocPtr = 0,
        linenumPtr = 0,
        relocations = 0,
        lineNumbers = 0,
        characteristics = Characteristic.CODE.id |
          Characteristic.EXECUTE.id |
          Characteristic.READ.id) 
      , code)
   
    val dataSection = Section(
      SectionHeader(
        name = "data",
        virtualSize = rawData.length,
        virtualAddress = 0x2000,
        sizeOfRawData = 0x200,
        pointerToRawData = 0x600,
        relocPtr = 0,
        linenumPtr = 0,
        relocations = 0,
        lineNumbers = 0,
        characteristics = Characteristic.INITIALIZED.id |
          Characteristic.READ.id |
          Characteristic.WRITE.id)
      , rawData)
      
    val idataSection = Section(
      SectionHeader(
        name = ".idata",
        virtualSize = executableImports.rawData.length,
        virtualAddress = 0x3000,
        sizeOfRawData = 0x200,
        pointerToRawData = 0x800,
        relocPtr = 0,
        linenumPtr = 0,
        relocations = 0,
        lineNumbers = 0,
        characteristics = Characteristic.CODE.id |
          Characteristic.EXECUTE.id |
          Characteristic.READ.id)
     , executableImports.rawData)
      
    val standardSections = List(codeSection, dataSection, idataSection)

    val resourceSection: Option[Section] = resources map {res =>
      Option(Section(
       SectionHeader(
        name = ".rsrc",
        virtualSize = res.length,
        virtualAddress = 0x3000,
        sizeOfRawData = res.length,
        pointerToRawData = 0x600,
        relocPtr = 0,
        linenumPtr = 0,
        relocations = 0,
        lineNumbers = 0,
        characteristics = Characteristic.INITIALIZED.id |
          Characteristic.READ.id), res))
    } getOrElse None

    val sections: List[Section] = standardSections ++ resourceSection

    val optionalHeader = OptionalHeader(
      magic = if (is64Bit) 0x20b else 0x10b,
      majorLinkerVersion = 0,
      minorLinkerVersion = 40,
      sizeOfCode = 512,
      sizeOfInitializedData = 1024,
      sizeOfUninitData = 0,
      addressOfEntryPoint = 0x1000,
      baseOfCode = 0x1000,
      baseOfData = 0x2000,

      AdditionalFields(
        imageBase = 0x400000,
        sectionAlignment = 0x1000,
        fileAlignment = 0x200,
        majorOperatingSystemVersion = if (is64Bit) 5 else 4,
        minorOperatingSystemVersion = 2,
        majorImageVersion = 0,
        minorImageVersion = 0,
        majorSubsystemVersion = if (is64Bit) 5 else 4,
        minorSubsystemVersion = 2,
        win32Version = 0,
        sizeOfImage = 0x4000,
        sizeOfHeaders = 0x200,
        checksum = 0,
        subsystem = 3,
        dllCharacteristics = 0,
        sizeOfStackReserve = 0x100000,
        sizeOfStackCommit = if (is64Bit) 0x10000 else 0x1000,
        sizeOfHeapReserve = 0x100000,
        sizeOfHeapCommit = 0x1000,
        loaderFlags = 0,
        numberOfRvaAndSizes = 16))
        
    val numImportedFunctions = executableImports.importSymbols.filter(sym => !sym.name.contains(".dll")).size

    val directories = resources map {res => DataDirectories(
      importSymbols = executableImports.getImportsDirectory(0x3018),
      importAddressTable = executableImports.getIATDirectory(0x3054),
      resource = ImageDataDirectory(0x3000, 11300)) 
    } getOrElse {
        DataDirectories(
          importSymbols = executableImports.getImportsDirectory(idataSection.header.virtualAddress + numImportedFunctions * 6),
          importAddressTable = executableImports.getIATDirectory(idataSection.header.virtualAddress + numImportedFunctions * 6 + executableImports.nameTableSize))
    }

    val fileHeader = FileHeader(
      machine = if (is64Bit) 0x8664.toShort else 0x14C,
      numberOfSections = sections.size.toShort,
      timeDateStamp = 0x535BF29F,
      pointerToSymbolTable = 0, // no importance
      numberOfSymbols = 0, // no importance
      sizeOfOptionalHeader = if (is64Bit) 0xF0 else 0xE0,
      characteristics = if (is64Bit) 47 else 271)

    val peHeader = new NtHeader(fileHeader, optionalHeader)

    PortableExecutable64(dosHeader, peHeader, directories, sections)
  }
}