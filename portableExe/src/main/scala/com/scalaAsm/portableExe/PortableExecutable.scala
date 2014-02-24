package com.scalaAsm.portableExe

class PortableExecutable(val dosHeader: DosHeader,
                         val peHeader: PeHeader,
                         val optionalHeader: OptionalHeader,
                 		 val directories: DataDirectories,
                 		 val sections: CompiledSections,
                 		 val code: Array[Byte],
                 		 val rawData: Array[Byte],
                 		 val compiledImports: CompiledImports)
     {
	  	def get(): Array[Byte] = {
	  	  val result = ExeGenerator.align(ExeGenerator.align(dosHeader(),16,0) ++ peHeader() ++ optionalHeader() ++ directories() ++ sections.sections, optionalHeader.sizeOfCode, 0x00) ++
	  	               ExeGenerator.align(code, optionalHeader.fileAlignment, 0x00) ++
	  	               rawData ++
	  	               compiledImports.rawData
	  	  ExeGenerator.align(result, 0x100, 0x00)
	  	}
     }