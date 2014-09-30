package com.scalaAsm.coff

import java.nio.ByteBuffer
import java.nio.ByteOrder

sealed trait TypeIndicator
case object IMAGE_REL_AMD64_ABSOLUTE 

object TypeIndicator extends Enumeration {
  type indicator = Value
  val IMAGE_REL_AMD64_ABSOLUTE = Value(0x0000.toShort)
  val IMAGE_REL_AMD64_ADDR64   = Value(0x0001.toShort)
  val IMAGE_REL_AMD64_ADDR32   = Value(0x0002.toShort)
  val IMAGE_REL_AMD64_ADDR32NB = Value(0x0003.toShort) // RVA
  val IMAGE_REL_AMD64_REL32    = Value(0x0004.toShort)
  val IMAGE_REL_AMD64_REL32_1  = Value(0x0005.toShort)
  val IMAGE_REL_AMD64_REL32_2  = Value(0x0006.toShort)
  val IMAGE_REL_AMD64_REL32_3  = Value(0x0007.toShort)
  val IMAGE_REL_AMD64_REL32_4  = Value(0x0008.toShort)
  val IMAGE_REL_AMD64_REL32_5  = Value(0x0009.toShort)
  val IMAGE_REL_AMD64_SECTION  = Value(0x000A.toShort)
  val IMAGE_REL_AMD64_SECREL   = Value(0x000B.toShort)
  val IMAGE_REL_AMD64_SECREL7  = Value(0x000C.toShort)
  val IMAGE_REL_AMD64_TOKEN    = Value(0x200D.toShort)
  val IMAGE_REL_AMD64_SREL32   = Value(0x200E.toShort)
  val IMAGE_REL_AMD64_PAIR     = Value(0x200F.toShort)
  val IMAGE_REL_AMD64_SSPAN32  = Value(0x2010.toShort)
}

case class RelocationEntry(
    referenceAddress: Long,
    //symbolIndex: Long,
    symbolName: String, // replace with index
    reloationType: Short) {
  
  def apply() = {
    val bbuf = ByteBuffer.allocate(10)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    bbuf.putLong(referenceAddress)
    //bbuf.putLong(symbolIndex)
    bbuf.putShort(reloationType)
    bbuf.array()
  }
  
}