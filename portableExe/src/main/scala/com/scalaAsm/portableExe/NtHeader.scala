package com.scalaAsm.portableExe

private[portableExe] abstract class PeHeader(signature: String) {
  def fileHeader: FileHeader
  def optionalHeader: OptionalHeader
  
  def apply(): Array[Byte] = {
	  signature.toCharArray().map(_.toByte) ++ fileHeader() ++ optionalHeader()
  }
}

private[portableExe] class NtHeader(val fileHeader: FileHeader,
               val optionalHeader: OptionalHeader)
               extends PeHeader("PE\0\0")

private[portableExe] class DosFileHeader(val fileHeader: FileHeader,
                    val optionalHeader: OptionalHeader)
                    extends PeHeader("5A4D")

private[portableExe] class os2FileHeader(val fileHeader: FileHeader,
                    val optionalHeader: OptionalHeader)
                    extends PeHeader("454E")

private[portableExe] class os2LeFileHeader(val fileHeader: FileHeader,
                    val optionalHeader: OptionalHeader)
                    extends PeHeader("454C")