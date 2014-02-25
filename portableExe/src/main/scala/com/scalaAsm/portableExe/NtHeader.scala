package com.scalaAsm.portableExe

class NtHeader(val fileHeader: FileHeader, val optionalHeader: OptionalHeader) {
	val signature = "PE"
	  
	def apply(): Array[Byte] = {
	  signature.toCharArray().map(_.toByte) ++ Array[Byte](0,0) ++ fileHeader() ++ optionalHeader()
	}
}