package com.scalaAsm.portableExe

import java.nio.ByteBuffer
import java.nio.ByteOrder

object OptionalHeader {
  def getOptionalHeader(input: ByteBuffer): OptionalHeader = {
    val magic = input.getShort()
    val header = OptionalHeader(
      magic = magic,
      majorLinkerVersion = input.get(),
      minorLinkerVersion = input.get(),
      sizeOfCode = input.getInt(),
      sizeOfInitializedData = input.getInt(),
      sizeOfUninitData = input.getInt(),
      addressOfEntryPoint = input.getInt(),
      baseOfCode = input.getInt(),
      baseOfData = input.getInt(),

      AdditionalFields(
	      imageBase = input.getInt(),
	      sectionAlignment = input.getInt(),
	      fileAlignment = input.getInt(),
	      majorOperatingSystemVersion = input.getShort(),
	      minorOperatingSystemVersion = input.getShort(),
	      majorImageVersion = input.getShort(),
	      minorImageVersion = input.getShort(),
	      majorSubsystemVersion = input.getShort(),
	      minorSubsystemVersion = input.getShort(),
	      win32Version = input.getInt(),
	      sizeOfImage = input.getInt(),
	      sizeOfHeaders = input.getInt(),
	      checksum = input.getInt(),
	      subsystem = input.getShort(),
	      dllCharacteristics = input.getShort(),
	      sizeOfStackReserve = if (magic == 0x020B) input.getLong() else input.getInt(),
	      sizeOfStackCommit = if (magic == 0x020B) input.getLong() else input.getInt(),
	      sizeOfHeapReserve = if (magic == 0x020B) input.getLong() else input.getInt(),
	      sizeOfHeapCommit = if (magic == 0x020B) input.getLong() else input.getInt(),
	      loaderFlags = input.getInt(),
	      numberOfRvaAndSizes = input.getInt()
	  )
    )
    input.order(ByteOrder.LITTLE_ENDIAN)

    header
  }
}

/*
 *  magic: Short = 0x10b,
  majorLinkerVersion: Byte = 2,
  minorLinkerVersion: Byte = 50,
  sizeOfCode: Int = 512,
  sizeOfInitializedData: Int = 512,
  sizeOfUninitData: Int = 0,
  addressOfEntryPoint: Int = 0x1000
  baseOfCode: Int = 0x1000
  baseOfData: Int = 0x2000

  // Additional fields
  imageBase: Int = 0x400000
  sectionAlignment: Int = 0x1000
  fileAlignment: Int = 0x200
  majorOperatingSystemVersion: Short = 4
  minorOperatingSystemVersion: Short = 0
  majorImageVersion: Short = 0
  minorImageVersion: Short = 0
  majorSubsystemVersion: Short = 4
  minorSubsystemVersion: Short = 0
  win32Version: Int = 0
  sizeOfImage: Int = 0x3000
  sizeOfHeaders: Int = 0x200
  checksum: Int = 0
  subsystem: Short = 3
  dllCharacteristics: Short = 0
  sizeOfStackReserve: Long = 0x100000
  sizeOfStackCommit: Long = 0x1000
  sizeOfHeapReserve: Long = 0x100000
  sizeOfHeapCommit: Long = 0x1000
  loaderFlags: Int = 0
  numberOfRvaAndSizes: Int = 16
  directories: DataDirectories = null
 */

case class AdditionalFields(
  imageBase: Int,
  sectionAlignment: Int,
  fileAlignment: Int,
  majorOperatingSystemVersion: Short,
  minorOperatingSystemVersion: Short,
  majorImageVersion: Short,
  minorImageVersion: Short,
  majorSubsystemVersion: Short,
  minorSubsystemVersion: Short,
  win32Version: Int,
  sizeOfImage: Int,
  sizeOfHeaders: Int,
  checksum: Int,
  subsystem: Short,
  dllCharacteristics: Short,
  sizeOfStackReserve: Long,
  sizeOfStackCommit: Long,
  sizeOfHeapReserve: Long,
  sizeOfHeapCommit: Long,
  loaderFlags: Int,
  numberOfRvaAndSizes: Int 
)

case class OptionalHeader(
  magic: Short,
  majorLinkerVersion: Byte,
  minorLinkerVersion: Byte,
  sizeOfCode: Int,
  sizeOfInitializedData: Int,
  sizeOfUninitData: Int,
  addressOfEntryPoint: Int,
  baseOfCode: Int,
  baseOfData: Int,
  additionalFields: AdditionalFields) {

 // def size: Int = 96 + 15 * 8

  def apply() = {
    val bbuf = ByteBuffer.allocate(200)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    bbuf.putShort(magic)
    bbuf.put(majorLinkerVersion.toByte)
    bbuf.put(minorLinkerVersion.toByte)
    bbuf.putInt(sizeOfCode)
    bbuf.putInt(sizeOfInitializedData)
    bbuf.putInt(sizeOfUninitData)
    bbuf.putInt(addressOfEntryPoint)
    bbuf.putInt(baseOfCode)
    bbuf.putInt(baseOfData)
    bbuf.putInt(additionalFields.imageBase)
    bbuf.putInt(additionalFields.sectionAlignment)
    bbuf.putInt(additionalFields.fileAlignment)
    bbuf.putShort(additionalFields.majorOperatingSystemVersion)
    bbuf.putShort(additionalFields.minorOperatingSystemVersion)
    bbuf.putShort(additionalFields.majorImageVersion)
    bbuf.putShort(additionalFields.minorImageVersion)
    bbuf.putShort(additionalFields.majorSubsystemVersion)
    bbuf.putShort(additionalFields.minorSubsystemVersion)
    bbuf.putInt(additionalFields.win32Version)
    bbuf.putInt(additionalFields.sizeOfImage)
    bbuf.putInt(additionalFields.sizeOfHeaders)
    bbuf.putInt(additionalFields.checksum)
    bbuf.putShort(additionalFields.subsystem)
    bbuf.putShort(additionalFields.dllCharacteristics)
    if (magic == 0x020B) { // 64 bit
	    bbuf.putLong(additionalFields.sizeOfStackReserve)
	    bbuf.putLong(additionalFields.sizeOfStackCommit)
	    bbuf.putLong(additionalFields.sizeOfHeapReserve)
	    bbuf.putLong(additionalFields.sizeOfHeapCommit)
    } else {
	    bbuf.putInt(additionalFields.sizeOfStackReserve.toInt)
	    bbuf.putInt(additionalFields.sizeOfStackCommit.toInt)
	    bbuf.putInt(additionalFields.sizeOfHeapReserve.toInt)
	    bbuf.putInt(additionalFields.sizeOfHeapCommit.toInt)     
    }

    bbuf.putInt(additionalFields.loaderFlags)
    bbuf.putInt(additionalFields.numberOfRvaAndSizes)
    bbuf.array().take(bbuf.position())
  }
}