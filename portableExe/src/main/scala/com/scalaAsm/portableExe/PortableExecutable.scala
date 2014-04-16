package com.scalaAsm.portableExe


case class PortableExecutable(dosHeader: DosHeader,
						      peHeader: NtHeader,
                 		      directories: DataDirectories,
                 		      sections: Seq[SectionHeader],
                 		      code: Array[Byte],
                 		      rawData: Array[Byte],
                 		      compiledImports: CompiledImports)
 {
  	def get(): Array[Byte] = {
  	  val result = ExeGenerator.align(ExeGenerator.align(dosHeader(),16,0) ++
  	                                  peHeader() ++
  	                                  directories() ++
  	                                  sections.map(_.write).reduce(_ ++ _), 
  	                                  peHeader.optionalHeader.sizeOfCode, 0x00) ++
  	               ExeGenerator.align(code, peHeader.optionalHeader.additionalFields.fileAlignment, 0x00) ++
  	               rawData ++
  	               compiledImports.rawData
  	  ExeGenerator.align(result, 0x100, 0x00)
  	}
  	
  	override def toString = {
  	  dosHeader.toString + "\n" +
  	  peHeader.toString + "\n" +
  	  directories.toString + "\n" +
  	  sections.toString + "\n"
  	}
  	
 }