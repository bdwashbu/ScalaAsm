package com.scalaAsm.portableExe

import java.nio.ByteBuffer
import java.nio.ByteOrder

object PeHeader {
  def getPeHeader(input: ByteBuffer): PeHeader = {
    input.order(ByteOrder.LITTLE_ENDIAN)
    val magicNumber = (0 until 4) map (x => input.get.toChar) mkString
    val fHeader = FileHeader.getFileHeader(input)
    val oHeader = OptionalHeader.getOptionalHeader(input)
    val peHeader = new PeHeader(magicNumber) {
      def fileHeader = fHeader
      def optionalHeader = oHeader
    }
    peHeader
  }
}

private[portableExe] abstract class PeHeader(val signature: String) {
  def fileHeader: FileHeader
  def optionalHeader: OptionalHeader
  
  def apply(): Array[Byte] = {
	  signature.toCharArray().map(_.toByte) ++: fileHeader() ++: optionalHeader()
  }
}

case class NtHeader(val fileHeader: FileHeader,
               val optionalHeader: OptionalHeader)
               extends PeHeader("PE\u0000\u0000")

private[portableExe] class DosFileHeader(val fileHeader: FileHeader,
                    val optionalHeader: OptionalHeader)
                    extends PeHeader("5A4D")

private[portableExe] class os2FileHeader(val fileHeader: FileHeader,
                    val optionalHeader: OptionalHeader)
                    extends PeHeader("454E")

private[portableExe] class os2LeFileHeader(val fileHeader: FileHeader,
                    val optionalHeader: OptionalHeader)
                    extends PeHeader("454C")