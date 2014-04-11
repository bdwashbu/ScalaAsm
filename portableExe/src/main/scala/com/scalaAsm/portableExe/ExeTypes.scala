package com.scalaAsm.portableExe

import java.io.{ OutputStream }
import java.io.DataOutputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.collection.mutable.LinkedHashMap
import scala.collection.immutable.TreeMap
import java.io.ByteArrayOutputStream

private trait ExeWriter {

    def write(stream: DataOutputStream)
  
    def writePadding(stream: DataOutputStream, numBytes: Int) = {
      stream.write(Array.fill[Byte](numBytes)(0))
    }

    def hex2Bytes(hex: String) = {
	    def stripChars(s: String, ch: String) = s filterNot (ch contains _)
	    stripChars(hex, " -").grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
	  }

    def write(stream: OutputStream, value: Short) = {
      val buffer = ByteBuffer.allocate(2)
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      buffer.putShort(value)
      stream.write(buffer.array())
    }

    def write(stream: OutputStream, value: Int) = {
      val buffer = ByteBuffer.allocate(4)
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      buffer.putInt(value)
      stream.write(buffer.array())
    }

    def write(stream: OutputStream, value: Char) = {
      val buffer = ByteBuffer.allocate(2)
      buffer.putChar(value)
      stream.write(buffer.array()(1))
    }

    def write(stream: OutputStream, value: String) = {
      val buffer = ByteBuffer.allocate(value.size)
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      buffer.put(value.getBytes())
      stream.write(buffer.array())
    }
  }


  
  