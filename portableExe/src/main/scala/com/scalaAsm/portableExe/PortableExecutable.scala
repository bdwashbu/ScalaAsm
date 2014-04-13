package com.scalaAsm.portableExe


class PortableExecutable(val dosHeader: DosHeader,
						 val ntHeader: NtHeader,
                 		 val directories: DataDirectories,
                 		 val sections: Seq[SectionHeader],
                 		 val code: Array[Byte],
                 		 val rawData: Array[Byte],
                 		 val compiledImports: CompiledImports)
 {
  	def get(): Array[Byte] = {
  	  val result = ExeGenerator.align(ExeGenerator.align(dosHeader(),16,0) ++
  	                                  ntHeader() ++
  	                                  directories() ++
  	                                  sections.map(_.write).reduce(_ ++ _), 
  	                                  ntHeader.optionalHeader.sizeOfCode, 0x00) ++
  	               ExeGenerator.align(code, ntHeader.optionalHeader.fileAlignment, 0x00) ++
  	               rawData ++
  	               compiledImports.rawData
  	  ExeGenerator.align(result, 0x100, 0x00)
  	}
 }