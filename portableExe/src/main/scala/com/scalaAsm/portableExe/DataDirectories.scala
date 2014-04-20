package com.scalaAsm.portableExe

import java.io.DataOutputStream
import scala.collection.immutable.TreeMap
import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.io.FileOutputStream
import sections._

private[portableExe] case class ImageDataDirectory(virtualAddress: Int, size: Int) extends ExeWriter {
  def write(stream: DataOutputStream) {
    write(stream, virtualAddress)
    write(stream, size)
  }
}

object DataDirectories {
  def getDirectories(input: ByteBuffer): DataDirectories = {
    DataDirectories(
	    exportSymbols = ImageDataDirectory(input.getInt, input.getInt),
	    importSymbols = ImageDataDirectory(input.getInt, input.getInt),
	    resource = ImageDataDirectory(input.getInt, input.getInt),
	    exception = ImageDataDirectory(input.getInt, input.getInt),
	    security = ImageDataDirectory(input.getInt, input.getInt),
	    baseRelocation = ImageDataDirectory(input.getInt, input.getInt),
	    debug = ImageDataDirectory(input.getInt, input.getInt),
	    copyRight = ImageDataDirectory(input.getInt, input.getInt),
	    globalPtr = ImageDataDirectory(input.getInt, input.getInt),
	    tls = ImageDataDirectory(input.getInt, input.getInt),
	    loadConfig = ImageDataDirectory(input.getInt, input.getInt),
	    boundImport = ImageDataDirectory(input.getInt, input.getInt),
	    importAddressTable = ImageDataDirectory(input.getInt, input.getInt),
	    com = ImageDataDirectory(input.getInt, input.getInt),
	    delayedImport = ImageDataDirectory(input.getInt, input.getInt),
	    reserved = ImageDataDirectory(input.getInt, input.getInt))
  }
}

// Contains the addresses of all important data structures in the PE

private[portableExe] case class DataDirectories(
    exportSymbols: ImageDataDirectory = ImageDataDirectory(0,0),
    importSymbols: ImageDataDirectory = ImageDataDirectory(0,0),
    resource: ImageDataDirectory      = ImageDataDirectory(0,0),
    exception: ImageDataDirectory     = ImageDataDirectory(0,0),
    security: ImageDataDirectory      = ImageDataDirectory(0,0),
    baseRelocation: ImageDataDirectory = ImageDataDirectory(0,0),
    debug: ImageDataDirectory         = ImageDataDirectory(0,0),
    copyRight: ImageDataDirectory     = ImageDataDirectory(0,0),
    globalPtr: ImageDataDirectory     = ImageDataDirectory(0,0),
    tls: ImageDataDirectory           = ImageDataDirectory(0,0),
    loadConfig: ImageDataDirectory    = ImageDataDirectory(0,0),
    boundImport: ImageDataDirectory   = ImageDataDirectory(0,0),
    importAddressTable: ImageDataDirectory = ImageDataDirectory(0,0),
    com: ImageDataDirectory           = ImageDataDirectory(0,0),
    delayedImport: ImageDataDirectory = ImageDataDirectory(0,0),
    reserved: ImageDataDirectory      = ImageDataDirectory(0,0)) extends Function0[Array[Byte]] {
    
    def size: Int = {
      15 * 8
    }
      
    def apply(): Array[Byte] = {
      val directoryOutput = new ByteArrayOutputStream()
      val stream = new DataOutputStream(directoryOutput)
      
      def write(dir: Option[ImageDataDirectory]) = dir match {
        case Some(dir) => dir.write(stream)
        case None => ImageDataDirectory(0,0).write(stream)
      }
      
      exportSymbols.write(stream)
      importSymbols.write(stream)
      resource.write(stream)
      exception.write(stream)
      security.write(stream)
      baseRelocation.write(stream)
      debug.write(stream)
      copyRight.write(stream)
      globalPtr.write(stream)
      tls.write(stream)
      loadConfig.write(stream)
      boundImport.write(stream)
      importAddressTable.write(stream)
      com.write(stream)
      delayedImport.write(stream)
      reserved.write(stream)
      
      directoryOutput.toByteArray
    }
  }