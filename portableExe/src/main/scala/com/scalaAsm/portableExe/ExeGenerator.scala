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
  
 case class CompiledImports(rawData: Array[Byte],
                            boundImportSize: Int,
                            nameTableSize: Int,
                            imports: Map[String, Int],
                            externs: Map[String, Int]) {
  
  def getImportsDirectory(optionalHeader: OptionalHeader, dataSize: Int) = {
    
    val importsLocation = optionalHeader.addressOfData + dataSize
    Directory(importsLocation, boundImportSize)
  }
  
  def getIATDirectory(optionalHeader: OptionalHeader, dataSize: Int) = {
    
    val endOfData = optionalHeader.addressOfData + dataSize
    val IATlocation = endOfData + boundImportSize + nameTableSize
    Directory(IATlocation, nameTableSize)
  }  
}

object ExeGenerator extends Sections {
  
  def align(array: Array[Byte], to: Int, filler: Byte = 0xCC.toByte) = {
    val currentSize = array.size
    val numPadding = (to - (currentSize % to)) % to
    array ++ Array.fill(numPadding)(filler)
  }
  
  def compileImports(addressOfData: Int, dataSize: Int): CompiledImports = { 
    
    val test = Imports(externs = Seq(Extern("kernel32.dll", List("ExitProcess", "GetStdHandle", "WriteFile", "FlushConsoleInputBuffer", "Sleep"))),
                       nonExterns = Seq(Extern("msvcrt.dll", List("printf", "_kbhit", "_getch"))),
                       offset = addressOfData + dataSize)
    
    test.link
  }

  def compile(asm: Assembled): PortableExecutable = {

    val optionalHeader = new OptionalHeader
    val (rawData, variables) = AsmCompiler.compileData(optionalHeader.addressOfData, asm.data)
    val compiledImports = compileImports(optionalHeader.addressOfData, rawData.size)
    val code = AsmCompiler.compileAssembly(optionalHeader.imageBase, asm, compiledImports.imports, compiledImports.externs, variables)
    
    val directories = DataDirectories(imports = compiledImports.getImportsDirectory(optionalHeader, rawData.size),
                                      importAddressTable = compiledImports.getIATDirectory(optionalHeader, rawData.size))
    
    val sections = compileSections(code.size, rawData.size + compiledImports.rawData.size)
    
    val dosHeader = new DosHeader
    val fileHeader = new FileHeader(optionalHeader, directories)
    val ntHeader = new NtHeader(fileHeader, optionalHeader)
    
    new PortableExecutable(dosHeader, ntHeader, directories, sections, code, rawData, compiledImports)
  }
  
  
}