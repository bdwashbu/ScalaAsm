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
import com.scalaAsm.coff.Assembled
import java.io.File
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import com.scalaAsm.coff.Sections
import com.scalaAsm.coff.Section
import com.scalaAsm.coff.SectionHeader
import com.scalaAsm.coff.Characteristic

class Linker64 extends Linker {
  
  def compileImports(assembled: Assembled, addressOfData: Int, dlls: Seq[String], is64Bit: Boolean): CompiledImports = { 
    
    val dllImports = dlls flatMap { dll =>
	    val file = new File("C:/Windows/System32/" + dll);
	 
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
	
	    val export = ImageExportDirectory.getExports(bbuf, sections, dirs.exportSymbols)
	    val importedSymbols = export.functionNames intersect assembled.symbols.map(_.name).toSeq

	    if (importedSymbols.isEmpty)
	      None
	    else
	      Some(Extern(dll, importedSymbols))
    }
    
    val test = Imports(imports = dllImports, offset = 0x3000)

    test.generateImports(is64Bit) 
  }
  
  def link(assembled: Assembled, addressOfData: Int, is64Bit: Boolean, dlls: String*): PortableExecutable = {

    val executableImports = compileImports(assembled, addressOfData, dlls, is64Bit)
    
    var offset = 0x3000
    val symMap = executableImports.importSymbols.map { sym =>
      val result = (sym.name.trim, offset)
      if (!sym.name.contains(".dll")) {
        offset += 6
      }
      result
    }.toMap
    
    var code = assembled.getCode(addressOfData, baseOffset = 0x400000)
    
//    var parserPosition = 0
//      for (token <- compiledAsm.onePass) {
//        token match {
//          case InstructionToken(inst) => inst match {
//              case OneMachineCodeBuilder(addr(name)) =>
//                relocations += Relocation(parserPosition+2, varMap(name) - 0x2000 - parserPosition - 7, name, 0)
//              case TwoMachineCodeBuilder(addr(name), _) =>
//                relocations += Relocation(parserPosition+2, varMap(name) - 0x2000 - parserPosition - 7, name, 0)
//              case TwoMachineCodeBuilder(_, addr(name)) =>
//                relocations += Relocation(parserPosition+2, varMap(name) - 0x2000 - parserPosition - 7, name, 0)
//              case _ =>
//            }
//          case ProcRef(name) =>
//            relocations += Relocation(parserPosition, refSymbols(name) - parserPosition - 5, name, 0)
//          case InvokeRef(name) =>
//            relocations += Relocation(parserPosition, refSymbols(name) - (parserPosition + 0x1000) - 5, name, 0)
//          case VarRef(name) =>
//            relocations += Relocation(parserPosition, refSymbols(name) + baseOffset - 0x1000, name, 0)
//          case JmpRefResolved(name) =>
//            relocations += Relocation(parserPosition, refSymbols(name) + baseOffset, name, 0)
//          case ImportRef(name) =>
//            relocations += Relocation(parserPosition, refSymbols(name) - (parserPosition + 0x1000) - 5, name, 0)
//          case LabelRef(name, inst, format) =>
//            relocations += Relocation(parserPosition, (refSymbols(name) - parserPosition - 2).toByte, name, 1)
//          case _ =>
//        }
//        token match {
//          case sizedToken: SizedToken => parserPosition += sizedToken.size
//          case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
//          case x: LabelRef => parserPosition += 2
//          case _ =>
//        }
//      }
    
    val newRefSymbols = assembled.refSymbols ++ symMap

    assembled.relocations.toList.foreach { relocation =>
      
      if (relocation.reloationType != 6) {
        val bb = java.nio.ByteBuffer.allocate(4)
        bb.order(ByteOrder.LITTLE_ENDIAN)
        
        relocation.reloationType match {
          case 1 =>
            bb.putInt(assembled.varMap(relocation.symbolName) + addressOfData - 0x2000 - relocation.referenceAddress - 5)
            code = code.patch(relocation.referenceAddress + 1, bb.array(), 4)
          case 2 =>
            bb.putInt(newRefSymbols(relocation.symbolName) - relocation.referenceAddress - 5)
            code = code.patch(relocation.referenceAddress + 1, bb.array(), 4)
          case 3 =>
            bb.putInt(newRefSymbols(relocation.symbolName) - (relocation.referenceAddress + 0x1000) - 5)
            code = code.patch(relocation.referenceAddress + 1, bb.array(), 4)
          case 4 =>
            bb.putInt(newRefSymbols(relocation.symbolName) - 0x1000 + addressOfData + 0x400000)
            code = code.patch(relocation.referenceAddress + 1, bb.array(), 4)
          case 5 =>
            bb.putInt(newRefSymbols(relocation.symbolName) + 0x400000)
            code = code.patch(relocation.referenceAddress + 1, bb.array(), 4)
          case 6 =>
            val bb1 = java.nio.ByteBuffer.allocate(1)
            bb1.order(ByteOrder.LITTLE_ENDIAN)
            bb1.put((newRefSymbols(relocation.symbolName) - relocation.referenceAddress - 2).toByte)
            code = code.patch(relocation.referenceAddress + 1, bb1.array(), 1)
        }
     } else {
       val bb1 = java.nio.ByteBuffer.allocate(1)
            bb1.order(ByteOrder.LITTLE_ENDIAN)
            bb1.put((newRefSymbols(relocation.symbolName) - relocation.referenceAddress - 2).toByte)
            code = code.patch(relocation.referenceAddress + 1, bb1.array(), 1)
       // code(relocation.referenceAddress.toInt + 1) = (assembled.varMap(relocation.symbolName) + addressOfData - 0x2000 - relocation.referenceAddress - 5).toByte
     } 
    }
    val resources = assembled.iconPath map (path => Option(ResourceGen.compileResources(0x3000, path))) getOrElse None

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
      , code.toArray)
   
    val dataSection = Section(
      SectionHeader(
        name = "data",
        virtualSize = assembled.rawData.length,
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
      , assembled.rawData)
      
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