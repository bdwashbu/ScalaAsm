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
  
  private def compileImports(addressOfData: Int, dataSize: Int): CompiledImports = { 
    
    val test = Imports(imports = Seq(Extern("kernel32.dll", List("ExitProcess", "GetStdHandle", "WriteFile", "FlushConsoleInputBuffer", "Sleep")),
                                     Extern("msvcrt.dll", List("printf", "_kbhit", "_getch"))),
                       offset = addressOfData + dataSize)
    
    test.generateImports
  }

  def link(asm: Assembled, addressOfData: Int): PortableExecutable = {

    val (rawData, variables) = AsmCompiler.compileData(addressOfData, asm.data)
    val compiledImports = compileImports(addressOfData, rawData.size)
    val directories: DataDirectories = DataDirectories(importSymbols = compiledImports.getImportsDirectory(addressOfData, rawData.size),
                                      importAddressTable = compiledImports.getIATDirectory(addressOfData, rawData.size))
    
    val optionalHeader = new OptionalHeader()
    optionalHeader.directories = directories;
    
    val code = AsmCompiler.compileAssembly(optionalHeader.imageBase, asm, compiledImports.imports, variables)
    
    
    
    val sections = compileSections(code.size, rawData.size + compiledImports.rawData.size)
    
    val dosHeader = new DosHeader
    val fileHeader = new FileHeader
    val ntHeader = new NtHeader(fileHeader, optionalHeader)
    
    new PortableExecutable(dosHeader, ntHeader, directories, sections, code, rawData, compiledImports)
  }
  
  
}