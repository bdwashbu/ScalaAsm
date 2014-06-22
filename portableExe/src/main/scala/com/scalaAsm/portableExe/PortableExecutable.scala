package com.scalaAsm.portableExe

import sections._

case class PortableExecutable(dosHeader: DosHeader,
						      peHeader: NtHeader,
                 		      directories: DataDirectories,
                 		      sections: Seq[SectionHeader],
                 		      code: Array[Byte],
                 		      rawData: Array[Byte],
                 		      compiledImports: CompiledImports,
                 		      resources: Array[Byte])
 {
  
  def align(array: Array[Byte], to: Int, filler: Byte = 0xCC.toByte) = {
    val currentSize = array.size
    val numPadding = (to - (currentSize % to)) % to
    array ++ Array.fill(numPadding)(filler)
  }
  
  	def get(): Array[Byte] = {
  	  val result = align(align(dosHeader(),16,0) ++
  	                                  peHeader() ++
  	                                  directories() ++
  	                                  sections.map(_.write).reduce(_ ++ _), 
  	                                  peHeader.optionalHeader.sizeOfCode, 0x00) ++
  	               align(code, peHeader.optionalHeader.additionalFields.fileAlignment, 0x00) ++
  	               align(rawData ++ compiledImports.rawData, peHeader.optionalHeader.additionalFields.fileAlignment, 0x00) ++
  	               resources
  	               
  	  align(result, peHeader.optionalHeader.additionalFields.fileAlignment, 0x00)
  	}
  	
  	override def toString = {
  	  dosHeader.toString + "\n" +
  	  peHeader.toString + "\n" +
  	  directories.toString + "\n" +
  	  sections.toString + "\n"
  	}
  	
 }