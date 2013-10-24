package com.scalaAsm.portableExe

import java.io.{ OutputStream }
import java.io.DataOutputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.collection.mutable.LinkedHashMap
import scala.collection.immutable.TreeMap
import java.io.ByteArrayOutputStream

trait ExeWriter {

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

	case class SectionHeader(val name: String,
      val virtualSize: Int,
      val virtualAddress: Int,
      val sizeOfRawData: Int,
      val pointerOrRawData: Int,
      val relocPtr: Int,
      val linenumPtr: Int,
      val relocations: Short,
      val lineNumbers: Short,
      val characteristics: Int) {

      def write: Array[Byte] = {
        val bbuf = ByteBuffer.allocate(256);
        bbuf.order(ByteOrder.LITTLE_ENDIAN)
        bbuf.put(name.padTo(8, 0.toChar).map(_.toByte).toArray)
        bbuf.putInt(virtualSize)
        bbuf.putInt(virtualAddress)
        bbuf.putInt(sizeOfRawData)
        bbuf.putInt(pointerOrRawData)
        bbuf.putInt(relocPtr)
        bbuf.putInt(linenumPtr)
        bbuf.putShort(relocations)
        bbuf.putShort(lineNumbers)
        bbuf.putInt(characteristics)
        bbuf.array().take(bbuf.position())
      }
    }

    object Characteristic extends Enumeration {
      type characteristic = Value
      val CODE = Value(0x00000020)
      val INITIALIZED = Value(0x00000040)
      val UNINIT_DATA = Value(0x00000080)
      val NOT_CACHE = Value(0x04000000)
      val NOT_PAGE = Value(0x08000000)
      val SHARED = Value(0x10000000)
      val EXECUTE = Value(0x20000000)
      val READ = Value(0x40000000)
      val WRITE = Value(0x80000000)
    }
  
  