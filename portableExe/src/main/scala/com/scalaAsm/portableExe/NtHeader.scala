package com.scalaAsm.portableExe

abstract class PeHeader(signature: String) {
  def fileHeader: FileHeader
  def optionalHeader: OptionalHeader
  
  def apply(): Array[Byte] = {
	  signature.toCharArray().map(_.toByte) ++ fileHeader() ++ optionalHeader()
  }
}

class NtHeader(val fileHeader: FileHeader,
               val optionalHeader: OptionalHeader)
               extends PeHeader("PE\0\0")

class DosFileHeader(val fileHeader: FileHeader,
                    val optionalHeader: OptionalHeader)
                    extends PeHeader("5A4D")

class os2FileHeader(val fileHeader: FileHeader,
                    val optionalHeader: OptionalHeader)
                    extends PeHeader("454E")

class os2LeFileHeader(val fileHeader: FileHeader,
                    val optionalHeader: OptionalHeader)
                    extends PeHeader("454C")