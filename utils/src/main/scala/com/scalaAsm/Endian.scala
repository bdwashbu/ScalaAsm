package com.scalaAsm.utils

import java.nio.ByteBuffer
import java.nio.ByteOrder

object Endian {
	 def swap(x: Int): Array[Byte] = {
      val buffer = ByteBuffer.allocate(4)
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      buffer.putInt(x)
      buffer.array()
    }

   def swap(x: Short): Array[Byte] = {
      val buffer = ByteBuffer.allocate(2)
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      buffer.putShort(x)
      buffer.array()
    }
}