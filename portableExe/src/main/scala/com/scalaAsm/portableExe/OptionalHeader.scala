package com.scalaAsm.portableExe

import java.nio.ByteBuffer
import java.nio.ByteOrder

object OptionalHeader {
  def getOptionalHeader(input: ByteBuffer): OptionalHeader = {
    val header = new OptionalHeader {
      magic = input.getShort()
      majorLinkerVersion = input.get()
      minorLinkerVersion = input.get()
      sizeOfCode = input.getInt()
      sizeOfInitializedData = input.getInt()
      sizeOfUninitData = input.getInt()
      addressOfEntryPoint = input.getInt()
      baseOfCode = input.getInt()
      baseOfData = input.getInt()

      // Additional fields
      imageBase = input.getInt()
      sectionAlignment = input.getInt()
      fileAlignment = input.getInt()
      majorOperatingSystemVersion = input.getShort()
      minorOperatingSystemVersion = input.getShort()
      majorImageVersion = input.getShort()
      minorImageVersion = input.getShort()
      majorSubsystemVersion = input.getShort()
      minorSubsystemVersion = input.getShort()
      win32Version = input.getInt()
      sizeOfImage = input.getInt()
      sizeOfHeaders = input.getInt()
      checksum = input.getInt()
      subsystem = input.getShort()
      dllCharacteristics = input.getShort()
      if (magic == 0x020B) { // 64 bit
	      sizeOfStackReserve = input.getLong()
	      sizeOfStackCommit = input.getLong()
	      sizeOfHeapReserve = input.getLong()
	      sizeOfHeapCommit = input.getLong()
      } else {
  	      sizeOfStackReserve = input.getInt()
	      sizeOfStackCommit = input.getInt()
	      sizeOfHeapReserve = input.getInt()
	      sizeOfHeapCommit = input.getInt()      
      }
      loaderFlags = input.getInt()
      numberOfRvaAndSizes = input.getInt()
      directories = null
    }
    input.order(ByteOrder.LITTLE_ENDIAN)

    header
  }
}

class OptionalHeader {
  var directories: DataDirectories = null
  
  var magic: Short = 0x10b
  var majorLinkerVersion: Byte = 2
  var minorLinkerVersion: Byte = 50
  var sizeOfCode: Int = 512
  var sizeOfInitializedData: Int = 512
  var sizeOfUninitData: Int = 0
  var addressOfEntryPoint: Int = 0x1000
  var baseOfCode: Int = 0x1000
  var baseOfData: Int = 0x2000

  // Additional fields
  var imageBase: Int = 0x400000
  var sectionAlignment: Int = 0x1000
  var fileAlignment: Int = 0x200
  var majorOperatingSystemVersion: Short = 4
  var minorOperatingSystemVersion: Short = 0
  var majorImageVersion: Short = 0
  var minorImageVersion: Short = 0
  var majorSubsystemVersion: Short = 4
  var minorSubsystemVersion: Short = 0
  var win32Version: Int = 0
  var sizeOfImage: Int = 0x3000
  var sizeOfHeaders: Int = 0x200
  var checksum: Int = 0
  var subsystem: Short = 3
  var dllCharacteristics: Short = 0
  var sizeOfStackReserve: Long = 0x100000
  var sizeOfStackCommit: Long = 0x1000
  var sizeOfHeapReserve: Long = 0x100000
  var sizeOfHeapCommit: Long = 0x1000
  var loaderFlags: Int = 0
  var numberOfRvaAndSizes: Int = 16

  def size: Int = 96 + directories.size

  def apply() = {
    val bbuf = ByteBuffer.allocate(96);
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
    
    bbuf.putInt(imageBase)
    bbuf.putInt(sectionAlignment)
    bbuf.putInt(fileAlignment)
    bbuf.putShort(majorOperatingSystemVersion)
    bbuf.putShort(minorOperatingSystemVersion)
    bbuf.putShort(majorImageVersion)
    bbuf.putShort(minorImageVersion)
    bbuf.putShort(majorSubsystemVersion)
    bbuf.putShort(minorSubsystemVersion)
    bbuf.putInt(win32Version)
    bbuf.putInt(sizeOfImage)
    bbuf.putInt(sizeOfHeaders)
    bbuf.putInt(checksum)
    bbuf.putShort(subsystem)
    bbuf.putShort(dllCharacteristics)
    if (magic == 0x020B) { // 64 bit
	    bbuf.putLong(sizeOfStackReserve)
	    bbuf.putLong(sizeOfStackCommit)
	    bbuf.putLong(sizeOfHeapReserve)
	    bbuf.putLong(sizeOfHeapCommit)
    } else {
	    bbuf.putInt(sizeOfStackReserve.toInt)
	    bbuf.putInt(sizeOfStackCommit.toInt)
	    bbuf.putInt(sizeOfHeapReserve.toInt)
	    bbuf.putInt(sizeOfHeapCommit.toInt)     
    }

    bbuf.putInt(loaderFlags)
    bbuf.putInt(numberOfRvaAndSizes)
    bbuf.array()
  }
}