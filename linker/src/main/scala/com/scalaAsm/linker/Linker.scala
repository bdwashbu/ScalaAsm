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

abstract class Linker {
  def is64Bit: Boolean
  def link(executableImports: CompiledImports, code: Array[Byte],
    addressOfData: Int, rawData: Array[Byte], resources: Option[Array[Byte]]): PortableExecutable
}

class Linker32 extends Linker {
  
  import java.nio.ByteBuffer
  import java.nio.ByteOrder
  import com.scalaAsm.asm.CodeSection

  def is64Bit = false
  
  def link(executableImports: CompiledImports, code: Array[Byte],
    addressOfData: Int, rawData: Array[Byte], resources: Option[Array[Byte]]): PortableExecutable = {

    val rawDataSize = rawData.length

    // Standard real-mode dos program that prints an error if it cannot be run
    
    val dosWarning = "This program cannot be run in DOS mode.\r\r\n$"
    def getStub: Array[Byte] = {
      import com.scalaAsm.x86.Instructions.Standard._
      val dosStub = new CodeSection {
        builder += push(cs)
        builder += pop(ds)
        builder += mov(dx, word(0xE.toByte))
        builder += mov(ah, byte(0x9))
        builder += int(byte(0x21))
        builder += mov(ax, word(0x4C01))
        builder += int(byte(0x21))
      }
  
      val bbuf = ByteBuffer.allocate(dosStub.getRawBytes.length + dosWarning.length);
      bbuf.put(dosStub.getRawBytes)
      bbuf.put(dosWarning.toCharArray() map (_.toByte))
      bbuf.array()
    }
    
    val dosHeader = DosHeader(
      e_magic = "MZ",
      e_cblp = 144,
      e_cp = 3,
      e_crlc = 0,
      e_cparhdr = 4,
      e_minalloc = 0,
      e_maxalloc = 65535.toShort,
      e_ss = 0,
      e_sp = 184,
      e_csum = 0,
      e_ip = 0,
      e_cs = 0,
      e_lfarlc = 64,
      e_ovno = 0,
      e_res = (0,0,0,0),
      e_oemid = 0,
      e_oeminfo = 0,
      e_res2 = (0,0,0,0,0,0,0,0,0,0),
      e_lfanew = 128,
      dosStub = getStub)

    val standardSections = List(
      SectionHeader(
        name = ".text",
        virtualSize = code.size,
        virtualAddress = 0x1000,
        sizeOfRawData = 0x200,
        pointerToRawData = 0x200,
        relocPtr = 0,
        linenumPtr = 0,
        relocations = 0,
        lineNumbers = 0,
        characteristics = Characteristic.CODE.id |
          Characteristic.EXECUTE.id |
          Characteristic.READ.id),

      SectionHeader(
        name = ".data",
        virtualSize = rawDataSize + executableImports.rawData.size,
        virtualAddress = 0x2000,
        sizeOfRawData = 0x200,
        pointerToRawData = 0x400,
        relocPtr = 0,
        linenumPtr = 0,
        relocations = 0,
        lineNumbers = 0,
        characteristics = Characteristic.INITIALIZED.id |
          Characteristic.READ.id |
          Characteristic.WRITE.id))

    val resourceSection: Option[SectionHeader] = resources map {res =>
      Option(SectionHeader(
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
          Characteristic.READ.id))
    } getOrElse None

    val sections: List[SectionHeader] = standardSections ++ resourceSection

    val optionalHeader = OptionalHeader(
      magic = 0x10b,
      majorLinkerVersion = 2,
      minorLinkerVersion = 50,
      sizeOfCode = 512,
      sizeOfInitializedData = 546,
      sizeOfUninitData = 0,
      addressOfEntryPoint = 0x1000,
      baseOfCode = 0x1000,
      baseOfData = 0x2000,

      AdditionalFields(
        imageBase = 0x400000,
        sectionAlignment = 0x1000,
        fileAlignment = 0x200,
        majorOperatingSystemVersion = 4,
        minorOperatingSystemVersion = 0,
        majorImageVersion = 0,
        minorImageVersion = 0,
        majorSubsystemVersion = 4,
        minorSubsystemVersion = 0,
        win32Version = 0,
        sizeOfImage = sections.last.virtualAddress + sections.last.virtualSize,
        sizeOfHeaders = 0x200,
        checksum = 0,
        subsystem = 3,
        dllCharacteristics = 0,
        sizeOfStackReserve = 0x100000,
        sizeOfStackCommit = 0x1000,
        sizeOfHeapReserve = 0x100000,
        sizeOfHeapCommit = 0x1000,
        loaderFlags = 0,
        numberOfRvaAndSizes = 16))

    val directories = resources map {res => DataDirectories(
      importSymbols = executableImports.getImportsDirectory(addressOfData + rawDataSize),
      importAddressTable = executableImports.getIATDirectory(addressOfData + rawDataSize + executableImports.boundImportSize + executableImports.nameTableSize),
      resource = ImageDataDirectory(0x3000, 11300)) 
    } getOrElse
      DataDirectories(
        importSymbols = executableImports.getImportsDirectory(addressOfData + rawDataSize),
        importAddressTable = executableImports.getIATDirectory(addressOfData + rawDataSize + executableImports.boundImportSize + executableImports.nameTableSize))

    val fileHeader = FileHeader(
      machine = 0x14C,
      numberOfSections = sections.size.toShort,
      timeDateStamp = 0x5132F2E5,
      pointerToSymbolTable = 0, // no importance
      numberOfSymbols = 0, // no importance
      sizeOfOptionalHeader = 0xE0,
      characteristics = 271)

    val peHeader = new NtHeader(fileHeader, optionalHeader)
    val res: Array[Byte] = resources getOrElse Array()
    PortableExecutable32(dosHeader, peHeader, directories, sections, code, rawData, executableImports, res)
  }
}