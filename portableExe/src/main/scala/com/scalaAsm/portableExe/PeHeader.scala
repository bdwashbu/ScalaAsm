package com.scalaAsm.portableExe

import java.nio.ByteBuffer
import java.nio.ByteOrder

class PeHeader(optionalHeader: OptionalHeader, directories: DataDirectories) {
    var signature = "PE"
    var machine: Short = 0x14C
    var numberOfSections: Short = 2
    var timeDateStamp: Int = 0x5132F2E5
    var pointerToSymbolTable: Int = _ // no importance
    var numberOfSymbols: Int = _ // no importance
    var sizeOfOptionalHeader: Short = (optionalHeader().length + directories().length).toShort
    var characteristics: Short = 271

    def apply() = {
      val bbuf = ByteBuffer.allocate(256);
      bbuf.order(ByteOrder.LITTLE_ENDIAN)
      bbuf.put(signature.toCharArray().map(_.toByte) ++ Array[Byte](0,0)) // end of signature is always 0x0000
      bbuf.putShort(machine)
      bbuf.putShort(numberOfSections)
      bbuf.putInt(timeDateStamp)
      bbuf.putInt(pointerToSymbolTable)
      bbuf.putInt(numberOfSymbols)
      bbuf.putShort(sizeOfOptionalHeader)
      bbuf.putShort(characteristics)
      bbuf.array().take(bbuf.position())
    }
  }