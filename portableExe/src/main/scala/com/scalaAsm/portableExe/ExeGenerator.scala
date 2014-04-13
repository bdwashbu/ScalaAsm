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
	    val dirs = DataDirectories.getDirectories(bbuf, peHeader.optionalHeader.numberOfRvaAndSizes)
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
    val directories: DataDirectories = DataDirectories(importSymbols = compiledImports.getImportsDirectory(addressOfData, rawData.size),
                                      importAddressTable = compiledImports.getIATDirectory(addressOfData, rawData.size))
    
    val dosHeader = new DosHeader
    val fileHeader = new FileHeader
    val optionalHeader = new OptionalHeader()
    optionalHeader.directories = directories;
    val peHeader = new NtHeader(fileHeader, optionalHeader)
    
    val code = AsmCompiler.finalizeAssembly(compiledAsm, variables, compiledImports.imports, optionalHeader.imageBase)
    
    val sections = compileSections(code.size, rawData.size + compiledImports.rawData.size)
    
    new PortableExecutable(dosHeader, peHeader, directories, sections, code, rawData, compiledImports)
  }
  
  
}