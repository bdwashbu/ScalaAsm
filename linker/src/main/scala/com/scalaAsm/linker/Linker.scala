package com.scalaAsm.linker

import com.scalaAsm.portableExe._
import com.scalaAsm.portableExe.sections._
import java.io.File
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import com.scalaAsm.coff.{ OptionalHeader => _, _ }
import scala.collection.mutable.ListBuffer
import com.scalaAsm.portableExe.FileHeader._

class Linker {

  def compileImports(objFile: Coff, dlls: Seq[String], is64Bit: Boolean, importsLoc: Int): CompiledImports = {

    val symbolsToBindTo = objFile.relocations.filter(reloc => reloc.symbol.sectionNumber == 0).map(_.symbol.name).toSeq

    val exportMap = dlls.map { dllName =>
      (dllName -> {
        val file = if (new File(dllName).exists) new File(dllName) else new File("C:/Windows/System32/" + dllName);

        val bFile: Array[Byte] = Array.fill(file.length().toInt)(0);

        //convert file into array of bytes
        val fileInputStream = new FileInputStream(file);
        fileInputStream.read(bFile);
        fileInputStream.close();

        val bbuf = ByteBuffer.wrap(bFile)
        bbuf.order(ByteOrder.LITTLE_ENDIAN)

        val dosHeader = DosHeader.getDosHeader(bbuf)
        val peHeader = PeHeader.getPeHeader(bbuf)
        val dirs = DataDirectories.getDirectories(bbuf)
        val sections = Sections.getSections(bbuf, peHeader.fileHeader.numberOfSections)

        ImageExportDirectory.getExports(bbuf, sections, dirs.exportSymbols.virtualAddress).functionNames
      })
    }.toMap

    val resolvedMap = symbolsToBindTo.map { sym => sym -> exportMap.filter { case (dll, exports) => exports.contains(sym) }.map { case (dll, exports) => dll } }.toMap

    resolvedMap.foreach { sym =>
      if (sym._2.size == 0) {
        throw new Exception(s"symbol not found: ${sym._1}")
      } else if (sym._2.size > 1) {
        throw new Exception(s"ambiguous symbol found: ${sym._1}")
      }
    }

    val dllMap = dlls.map { dllName => (dllName, resolvedMap.filter { case (key, values) => values.toSeq.contains(dllName) }.map { _._1 }) }.toMap

    val dllImports = dllMap.map {
      case (dllName, symbols) =>
        Extern(dllName, symbols.toSeq)
    }.toSeq

    val test = Imports(imports = dllImports, offset = importsLoc)

    test.generateImports(is64Bit)
  }

  def link(objFile: Coff, addressOfData: Int, is64Bit: Boolean, isDll: Boolean, dlls: String*): PortableExecutable = {

    val dosHeader = DosHeader(
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
      watermark = "Scala x86\0")

    lazy val fileHeader = {
      val addressSize: Short = if (is64Bit) LargeAddresses else is32Bit

      FileHeader(
        machine = if (is64Bit) AMD64 else Intel386,
        numberOfSections = peSections.size.toShort,
        timeDateStamp = 0x535BF29F,
        pointerToSymbolTable = 0, // no importance
        numberOfSymbols = 0, // no importance
        sizeOfOptionalHeader = if (is64Bit) 0xF0 else 0xE0,
        characteristics = if (isDll) (addressSize | DLL | Executable | symbolsStripped | linesStripped).toShort else (addressSize | Executable).toShort)
    }

    lazy val optionalHeader = OptionalHeader(
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
        sizeOfImage = peSections.last.header.virtualAddress + 0x1000,
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

    lazy val executableImports = compileImports(objFile, dlls, is64Bit, 0x3000)

    lazy val importSymbols = {
      var offset = 0x3000

      executableImports.importSymbols.flatMap { sym =>

        if (!sym.name.contains(".dll")) {
          val result = CoffSymbol(sym.name.trim, offset, 0, IMAGE_SYM_DTYPE_FUNCTION(0), IMAGE_SYM_CLASS_EXTERNAL, Nil)
          offset += 6
          Some(result)
        } else None
      }
    }

    lazy val getSymbolAddress: Map[String, Int] = {
      val newRefSymbols = objFile.symbols ++ importSymbols

      val vars = newRefSymbols.filter(sym => sym.storageClass == IMAGE_SYM_CLASS_EXTERNAL && sym.sectionNumber == 1) // TODO: 1 being data.  Make this dynamic

      val otherStuff = newRefSymbols.diff(vars)

      (vars ++ otherStuff).map { sym => (sym.name, sym.value) }.toMap
    }

    lazy val resources = objFile.iconPath map (path => Option(ResourceGen.compileResources(0x4000, path))) getOrElse None

    lazy val codeSection = objFile.sections.find { section => (section.header.characteristics & Characteristic.CODE.id) != 0 }.get
    lazy val dataSection = objFile.sections.find { section => (section.header.characteristics & Characteristic.WRITE.id) != 0 }.get

    lazy val exportData: Array[Byte] = if (isDll) {
      val exportSymbols = objFile.symbols.filter(sym => sym.storageClass == IMAGE_SYM_CLASS_EXTERNAL && sym.sectionNumber == 2)
      val exports = exportSymbols.map { sym => Export(sym.name, 0x1000 + sym.value) }
      
      ImageExportDirectory.writeExports("helloWorld.dll", exports.toSeq, 0x2000) // fix hardcoded!!!
    } else {
      Array()
    }

    lazy val idataSection = Section(
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
          Characteristic.READ.id), executableImports.rawData)

    lazy val standardSections = if (isDll)
      List(codeSection.copy(contents = code, header = codeSection.header.copy(sizeOfRawData = code.size)))
    else
      List(codeSection.copy(contents = code), dataSection, idataSection)

    lazy val peSections: Seq[Section] = {
      var virtAddress = 0x1000
      var rawPointer = 0x200
      
      val sections = ListBuffer[Section]() ++ standardSections

      if (objFile.iconPath.isDefined) {
        val res = ResourceGen.compileResources(0x4000, objFile.iconPath.get)
        sections += Section(
          SectionHeader(
            name = ".rsrc",
            virtualSize = res.length,
            virtualAddress = 0x4000,
            sizeOfRawData = res.length,
            pointerToRawData = 0xA00,
            relocPtr = 0,
            linenumPtr = 0,
            relocations = 0,
            lineNumbers = 0,
            characteristics = Characteristic.INITIALIZED.id |
              Characteristic.READ.id), res)
      }
  
      if (isDll) {
        sections += Section(
          SectionHeader(
            name = ".edata", // export data
            virtualSize = exportData.length,
            virtualAddress = 0x4000,
            sizeOfRawData = exportData.length,
            pointerToRawData = 0x800,
            relocPtr = 0,
            linenumPtr = 0,
            relocations = 0,
            lineNumbers = 0,
            characteristics = Characteristic.INITIALIZED.id |
              Characteristic.READ.id), exportData)
      }

      sections map { section =>
        val size: Int = (section.header.sizeOfRawData / 0x200) + 1
        val result = Section(
          SectionHeader(
            name = section.header.name,
            virtualSize = section.header.sizeOfRawData,
            virtualAddress = virtAddress,
            sizeOfRawData = size * 0x200,
            pointerToRawData = rawPointer,
            relocPtr = 0,
            linenumPtr = 0,
            relocations = 0,
            lineNumbers = 0,
            characteristics = section.header.characteristics), section.contents)

        rawPointer += size * 0x200
        val newVirtAddress: Int = ((size * 0x200) / 0x1000) + 1
        virtAddress += newVirtAddress * 0x1000
        result
      }
    }

    lazy val code = {
      var patchedCode = codeSection.contents
      objFile.relocations.toList.foreach { relocation =>

        val bb = java.nio.ByteBuffer.allocate(4)
        bb.order(ByteOrder.LITTLE_ENDIAN)

        relocation.relocationType match {
          case 1 => // addr
            bb.putInt(getSymbolAddress(relocation.symbol.name) + addressOfData - 0x2000 - relocation.referenceAddress - 5)
            patchedCode = patchedCode.patch(relocation.referenceAddress + 1, bb.array(), 4)
          case 2 => // ProcRef
            bb.putInt(getSymbolAddress(relocation.symbol.name) - relocation.referenceAddress - 5)
            patchedCode = patchedCode.patch(relocation.referenceAddress + 1, bb.array(), 4)
          case 20 => // InvokeRef/ImportRef
            bb.putInt(getSymbolAddress(relocation.symbol.name) - (relocation.referenceAddress + 0x1000) - 4)
            patchedCode = patchedCode.patch(relocation.referenceAddress, bb.array(), 4)
          case 6 => // GoAsm VarRef
            bb.putInt(getSymbolAddress(relocation.symbol.name) - 0x1000 + addressOfData + 0x400000)
            patchedCode = patchedCode.patch(relocation.referenceAddress, bb.array(), 4)
          case 7 => // LabelRef
            patchedCode(relocation.referenceAddress + 1) = (getSymbolAddress(relocation.symbol.name) - relocation.referenceAddress - 2).toByte
        }
      }
      patchedCode
    }

    val numImportedFunctions = executableImports.importSymbols.filter(sym => !sym.name.contains(".dll")).size

    val directories = DataDirectories(
      importSymbols = if (!isDll) executableImports.getImportsDirectory(idataSection.header.virtualAddress + numImportedFunctions * 6) else ImageDataDirectory(0, 0),
      importAddressTable = if (!isDll) executableImports.getIATDirectory(idataSection.header.virtualAddress + numImportedFunctions * 6 + executableImports.boundImportSize) else ImageDataDirectory(0, 0),
      resource = if (resources.isDefined) ImageDataDirectory(0x4000, 11300) else ImageDataDirectory(0, 0),
      exportSymbols = if (isDll) ImageDataDirectory(0x2000, exportData.size) else ImageDataDirectory(0, 0))

    PortableExecutable(dosHeader, NtHeader(fileHeader, optionalHeader), directories, peSections)
  }
}