package com.scalaAsm.portableExe

import java.io.{ OutputStream }
import java.nio.{ ByteBuffer, ByteOrder }
import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeMap
import java.io.DataOutputStream
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import com.scalaAsm.asm.Assembled
import com.scalaAsm.asm.AsmCompiler
import com.scalaAsm.utils.Endian
import java.io.File
import java.io.FileInputStream
import com.scalaAsm.asm.Tokens.ImportRef
  
private[portableExe] case class CompiledImports(rawData: Array[Byte],
                            boundImportSize: Int,
                            nameTableSize: Int,
                            imports: Map[String, Int]) {
  
  def getImportsDirectory(addressOfData: Int, dataSize: Int) = {
    val importsLocation = addressOfData + dataSize
    ImageDataDirectory(importsLocation, boundImportSize)
  }
  
  def getIATDirectory(addressOfData: Int, dataSize: Int) = {
    val endOfData = addressOfData + dataSize
    val IATlocation = endOfData + boundImportSize + nameTableSize
    ImageDataDirectory(IATlocation, nameTableSize)
  }  
}

object ExeGenerator extends Sections {
  
  def align(array: Array[Byte], to: Int, filler: Byte = 0xCC.toByte) = {
    val currentSize = array.size
    val numPadding = (to - (currentSize % to)) % to
    array ++ Array.fill(numPadding)(filler)
  }
  
  private def compileImports(addressOfData: Int, dataSize: Int, dlls: Seq[String], possibleFunctions: Seq[String]): CompiledImports = { 
    
    val dllImports = for (dll <- dlls) yield {
      println(dll)
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
	    val dirs = DataDirectories.getDirectories(bbuf, peHeader.optionalHeader.additionalFields.numberOfRvaAndSizes)
	    val sections = Sections.getSections(bbuf, peHeader.fileHeader.numberOfSections)
	
	    val export = ImageExportDirectory.getExports(bbuf, sections, dirs(0))

	    Extern(dll, export.functionNames intersect possibleFunctions)
    }
    
    val test = Imports(imports = dllImports,
                       offset = addressOfData + dataSize)
    println(test)
    test.generateImports
  }

  def link(asm: Assembled, addressOfData: Int, dlls: String*): PortableExecutable = {

    val (rawData, variables) = AsmCompiler.compileData(addressOfData, asm.data)
    
    val compiledAsm = AsmCompiler.compileAssembly(asm, variables)
    
    val unboundSymbols = compiledAsm.onePass.collect { case ImportRef(name) => name}
    
    val compiledImports = compileImports(addressOfData, rawData.size, dlls, unboundSymbols)

    val dosHeader = new DosHeader
    
    
    val optionalHeader = OptionalHeader(
      magic = 0x10b,
	  majorLinkerVersion = 2,
	  minorLinkerVersion = 50,
	  sizeOfCode = 512,
	  sizeOfInitializedData = 512,
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
		  sizeOfImage = 0x3000,
		  sizeOfHeaders = 0x200,
		  checksum = 0,
		  subsystem = 3,
		  dllCharacteristics = 0,
		  sizeOfStackReserve = 0x100000,
		  sizeOfStackCommit = 0x1000,
		  sizeOfHeapReserve = 0x100000,
		  sizeOfHeapCommit = 0x1000,
		  loaderFlags = 0,
		  numberOfRvaAndSizes = 16
	  )
    )
    
    val directories = DataDirectories(
      importSymbols = Some(compiledImports.getImportsDirectory(addressOfData, rawData.size)),
      importAddressTable = Some(compiledImports.getIATDirectory(addressOfData, rawData.size))
    )
    
    val code = AsmCompiler.finalizeAssembly(compiledAsm, variables, compiledImports.imports, optionalHeader.additionalFields.imageBase)
    
    val sections = compileSections(code.size, rawData.size + compiledImports.rawData.size)
    
    val fileHeader = FileHeader(
		machine = 0x14C,
	    numberOfSections = sections.size.toShort,
	    timeDateStamp = 0x5132F2E5,
	    pointerToSymbolTable = 0, // no importance
	    numberOfSymbols = 0, // no importance
	    sizeOfOptionalHeader = 0x0E0,
	    characteristics = 271
	)
	val peHeader = new NtHeader(fileHeader, optionalHeader)
    
    new PortableExecutable(dosHeader, peHeader, directories, sections, code, rawData, compiledImports)
  }
  
  
}