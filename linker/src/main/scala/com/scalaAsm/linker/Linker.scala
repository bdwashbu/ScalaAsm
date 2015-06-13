package com.scalaAsm.linker

import com.scalaAsm.portableExe.OptionalHeader
import com.scalaAsm.portableExe._
import com.scalaAsm.portableExe.sections._
import java.io.File
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import com.scalaAsm.coff.{OptionalHeader => _, _}
import scala.collection.mutable.ListBuffer
import FileHeader._

class Linker {
  
  def compileImports(objFile: Coff, dlls: Seq[String], is64Bit: Boolean, importsLoc: Int): CompiledImports = { 
    
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
	    val importedSymbols = export.functionNames intersect objFile.relocations.filter(reloc => reloc.symbol.sectionNumber == 0).map(_.symbol.name).toSeq
	    
	    if (importedSymbols.isEmpty)
	      None
	    else
	      Some(Extern(dll, importedSymbols))
    }
    
    val test = Imports(imports = dllImports, offset = importsLoc)

    test.generateImports(is64Bit) 
  }
  
  def link(objFile: Coff, addressOfData: Int, is64Bit: Boolean, isDll: Boolean, dlls: String*): PortableExecutable = {

    val executableImports = compileImports(objFile, dlls, is64Bit, 0x3000)

    var offset = 0x3000
    val importSymbols = executableImports.importSymbols.flatMap { sym => 
      if (!sym.name.contains(".dll")) {
        val result = CoffSymbol(sym.name.trim, offset, 0, IMAGE_SYM_DTYPE_FUNCTION(0), IMAGE_SYM_CLASS_EXTERNAL, Nil)
        offset += 6
        Some(result)
      } else None
    }

    val getSymbolAddress: Map[String, Int] = {
      val newRefSymbols = objFile.symbols ++ importSymbols
      
      val vars = newRefSymbols.filter(sym => sym.storageClass == IMAGE_SYM_CLASS_EXTERNAL && sym.sectionNumber == 1) // TODO: 1 being data.  Make this dynamic

      // shift everything over for KEEP
      
      val otherStuff = newRefSymbols.diff(vars)
      
      (vars ++ otherStuff).map{sym => (sym.name, sym.value)}.toMap
    }
    
    val resources = objFile.iconPath map (path => Option(ResourceGen.compileResources(0x4000, path))) getOrElse None

    val codeSection = objFile.sections.find { section => (section.header.characteristics & Characteristic.CODE.id) != 0 }.get
    val tempDataSection = objFile.sections.find { section => (section.header.characteristics & Characteristic.WRITE.id) != 0 }.get
    
    // Add room for KEEP
    val dataSection = tempDataSection.copy(contents = tempDataSection.contents)
    
     //val dataSection = objFile.sections.find { section => (section.header.characteristics & Characteristic.WRITE.id) != 0 }.get
    
    var code = codeSection.contents
    
    println(getSymbolAddress)
    
    objFile.relocations.toList.foreach { relocation =>
      
        val bb = java.nio.ByteBuffer.allocate(4)
        bb.order(ByteOrder.LITTLE_ENDIAN)
        
        relocation.relocationType match {
          case 1 => // addr
            bb.putInt(getSymbolAddress(relocation.symbol.name) + addressOfData - 0x2000 - relocation.referenceAddress - 5)
            code = code.patch(relocation.referenceAddress + 1, bb.array(), 4)
          case 2 => // ProcRef
            bb.putInt(getSymbolAddress(relocation.symbol.name) - relocation.referenceAddress - 5)
            code = code.patch(relocation.referenceAddress + 1, bb.array(), 4)
          case 20 => // InvokeRef/ImportRef
            bb.putInt(getSymbolAddress(relocation.symbol.name) - (relocation.referenceAddress + 0x1000) - 4)
            code = code.patch(relocation.referenceAddress, bb.array(), 4)
          case 6 => // GoAsm VarRef
            bb.putInt(getSymbolAddress(relocation.symbol.name) - 0x1000 + addressOfData + 0x400000)
            code = code.patch(relocation.referenceAddress, bb.array(), 4)
          case 7 => // LabelRef
            code(relocation.referenceAddress + 1) = (getSymbolAddress(relocation.symbol.name) - relocation.referenceAddress - 2).toByte
        }
    }
    
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
      
    val standardSections = List(codeSection.copy(contents = code), dataSection, idataSection)
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
    
    var virtAddress = 0x1000
    var rawPointer = 0x200
    val peSections: Seq[Section] = 
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
      
    val addressSize: Short = if (is64Bit) LargeAddresses else 0
      
    val fileHeader = FileHeader(
      machine = if (is64Bit) AMD64 else Intel386,
      numberOfSections = peSections.size.toShort,
      timeDateStamp = 0x535BF29F,
      pointerToSymbolTable = 0, // no importance
      numberOfSymbols = 0, // no importance
      sizeOfOptionalHeader = if (is64Bit) 0xF0 else 0xE0,
      characteristics = if (isDll) (addressSize | DLL).toShort else (addressSize | Executable).toShort)
      
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
        
    val numImportedFunctions = executableImports.importSymbols.filter(sym => !sym.name.contains(".dll")).size

    val directories = DataDirectories(
      importSymbols = executableImports.getImportsDirectory(idataSection.header.virtualAddress + numImportedFunctions * 6),
      importAddressTable = executableImports.getIATDirectory(idataSection.header.virtualAddress + numImportedFunctions * 6 + executableImports.nameTableSize),
      resource = if (resources.isDefined) ImageDataDirectory(0x4000, 11300) else ImageDataDirectory(0,0)) 

    PortableExecutable(dosHeader, NtHeader(fileHeader, optionalHeader), directories, peSections)
  }
}