package com.scalaAsm.linker

import com.scalaAsm.portableExe.CompiledImports
import java.io.File
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import com.scalaAsm.portableExe.DosHeader
import com.scalaAsm.portableExe.PeHeader
import com.scalaAsm.portableExe.DataDirectories
import com.scalaAsm.portableExe.sections.Sections
import com.scalaAsm.portableExe.sections.ImageExportDirectory
import com.scalaAsm.portableExe.sections.Extern
import com.scalaAsm.portableExe.sections.Imports
import com.scalaAsm.portableExe.PortableExecutable
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.portableExe.sections.ResourceGen


abstract class Assembled(val codeTokens: Seq[Any], val dataTokens: Seq[Token], val iconPath: Option[String] = None) {
  self =>
  val rawData: Array[Byte]
  val variables: (Int) => Map[String, Int]
  val compiledImports: (Int, Seq[String], Boolean) => CompiledImports
  
  case class CompiledAssembly(onePass: Seq[Token], positionPass: Seq[PostToken])
  
  def finalizeAssembly(variables: Map[String, Int], imports: Map[String, Int], imports64: Map[String, Int], baseOffset: Int): Array[Byte]

  def addIcon(path: String): Assembled = {
    new Assembled(codeTokens, dataTokens, Option(path)) {
      val rawData = self.rawData
      val variables = self.variables
      val compiledImports = self.compiledImports
      
      def finalizeAssembly(variables: Map[String, Int], imports: Map[String, Int], imports64: Map[String, Int], baseOffset: Int): Array[Byte] = {
        self.finalizeAssembly(variables, imports, imports64, baseOffset)
      }
    }
  }
  
  def compileImports(dataSize: Int, possibleFunctions: Seq[String]): (Int, Seq[String], Boolean) => CompiledImports = { 
    
    (addressOfData: Int, dlls: Seq[String], is64Bit: Boolean) => {
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
	    val importedSymbols = export.functionNames intersect possibleFunctions

	    if (importedSymbols.isEmpty)
	      None
	    else
	      Some(Extern(dll, importedSymbols))
    }
    
    val test = Imports(imports = dllImports,
                       offset = 0x3000)

    test.generateImports(is64Bit) 
    }
  }

  def link(addressOfData: Int, linker: Linker, is64Bit: Boolean, dlls: String*): PortableExecutable = {
    val executableImports = compiledImports(addressOfData, dlls, is64Bit)
    
    var offset = 0x3000
    val symMap = executableImports.importSymbols.map { sym =>
      val result = (sym.name.trim, offset)
      if (!sym.name.contains(".dll")) {
        offset += 6
      }
      result
    }.toMap
    
    val code = finalizeAssembly(variables(addressOfData), executableImports.imports, symMap, baseOffset = 0x400000 /*imagebase*/)

    val resources = iconPath map (path => Option(ResourceGen.compileResources(0x3000, path))) getOrElse None
    
    linker.link(executableImports, code, addressOfData, rawData, resources, is64Bit)
  }
}