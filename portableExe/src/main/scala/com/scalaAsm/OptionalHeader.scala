package com.scalaAsm.portableExe

import java.nio.ByteBuffer
import java.nio.ByteOrder

class OptionalHeader {
    val magic: Short = 0x10b
    var majorLinkerVersion: Char = 2
    var minorLinkerVersion: Char = 50
    var sizeOfCode: Int = 512
    var sizeOfInitializedData: Int = 512
    var sizeOfUninitData: Int = 0

    var addressOfEntryPoint: Int = 0x1000
    var addressOfCode: Int = 0x1000
    var addressOfData: Int = 0x2000

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
    var sizeOfStackReserve: Int = 0x100000
    var sizeOfStackCommit: Int = 0x1000
    var sizeOfHeapReserve: Int = 0x100000
    var sizeOfHeapCommit: Int = 0x1000
    var loaderFlags: Int = 0
    var NnumberOfRvaAndSizes: Int = 16

    def apply() = {
      val bbuf = ByteBuffer.allocate(512);
      bbuf.order(ByteOrder.LITTLE_ENDIAN)
      bbuf.putShort(magic)
      bbuf.put(majorLinkerVersion.toByte)
      bbuf.put(minorLinkerVersion.toByte)
      bbuf.putInt(sizeOfCode)
      bbuf.putInt(sizeOfInitializedData)
      bbuf.putInt(sizeOfUninitData)
      bbuf.putInt(addressOfEntryPoint)
      bbuf.putInt(addressOfCode)
      bbuf.putInt(addressOfData)
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
      bbuf.putInt(sizeOfStackReserve)
      bbuf.putInt(sizeOfStackCommit)
      bbuf.putInt(sizeOfHeapReserve)
      bbuf.putInt(sizeOfHeapCommit)
      bbuf.putInt(loaderFlags)
      bbuf.putInt(NnumberOfRvaAndSizes)
      bbuf.array().take(bbuf.position())
    }
  }