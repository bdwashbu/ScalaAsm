package com.scalaAsm.portableExe

import java.nio.ByteBuffer
import java.nio.ByteOrder

class FileHeader(optionalHeader: OptionalHeader) {
    var machine: Short = 0x14C
    var numberOfSections: Short = 2
    var timeDateStamp: Int = 0x5132F2E5
    var pointerToSymbolTable: Int = _ // no importance
    var numberOfSymbols: Int = _ // no importance
    var sizeOfOptionalHeader: Short = optionalHeader.size.toShort
    var characteristics: Short = 271

    def apply() = {
      val bbuf = ByteBuffer.allocate(20);
      bbuf.order(ByteOrder.LITTLE_ENDIAN)
      bbuf.putShort(machine)
      bbuf.putShort(numberOfSections)
      bbuf.putInt(timeDateStamp)
      bbuf.putInt(pointerToSymbolTable)
      bbuf.putInt(numberOfSymbols)
      bbuf.putShort(sizeOfOptionalHeader)
      bbuf.putShort(characteristics)
      bbuf.array()
    }
  }