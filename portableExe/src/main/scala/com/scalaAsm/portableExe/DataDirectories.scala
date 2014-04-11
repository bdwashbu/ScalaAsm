package com.scalaAsm.portableExe

import java.io.DataOutputStream
import scala.collection.immutable.TreeMap
import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer

private[portableExe] case class ImageDataDirectory(virtualAddress: Int, size: Int) extends ExeWriter {
  def write(stream: DataOutputStream) {
    write(stream, virtualAddress)
    write(stream, size)
  }
}

object DataDirectories {
  def getDirectories(input: ByteBuffer, numDirs: Int): Seq[ImageDataDirectory] = {
    for (i <- 0 until numDirs) yield ImageDataDirectory(input.getInt, input.getInt)
  }
}

object ImageExportDirectory {
  def getExports(input: ByteBuffer): ImageExportDirectory = {
    ImageExportDirectory(
    characteristics = input.getInt,
    timeDateStamp = input.getInt,
    majorVersion = input.getShort,
    minorVersion = input.getShort,
    name = input.getInt,
    base = input.getInt,
    numberOfFunctions = input.getInt,
    numberOfNames = input.getInt,
    addressOfFunctions = input.getInt,
    addressOfNames = input.getInt,
    addressOfNameOrdinals = input.getInt)
  }
}

case class ImageExportDirectory(
    characteristics: Int,
    timeDateStamp: Int,
    majorVersion: Short,
    minorVersion: Short,
    name: Int,
    base: Int,
    numberOfFunctions: Int,
    numberOfNames: Int,
    addressOfFunctions: Int,
    addressOfNames: Int,
    addressOfNameOrdinals: Int)

// Contains the addresses of all important data structures in the PE

private[portableExe] case class DataDirectories(importSymbols: ImageDataDirectory, importAddressTable: ImageDataDirectory) extends Function0[Array[Byte]]{
  
    object DirectoryTypes extends Enumeration {
      type characteristic = Value
      val ExportSymbols, ImportSymbols, Resource, Exception, Security, BaseRelocation, Debug, Copyright,
            GlobalPtr, TLS, LoadConfig, BoundImport, IAT, COM, DelayImport, Reserved = Value
    }

    import DirectoryTypes._
    
    // Virtual Address and Size
    private val directories = TreeMap(
      ExportSymbols -> None,
      ImportSymbols -> Some(importSymbols),
      Resource -> None,
      Exception -> None,
      Security -> None,
      BaseRelocation -> None,
      Debug -> None,
      Copyright -> None,
      GlobalPtr -> None,
      TLS -> None,
      LoadConfig -> None,
      BoundImport -> None,
      IAT -> Some(importAddressTable),
      COM -> None,
      DelayImport -> None,
      Reserved -> None)

    def size: Int = {
      directories.size * 8
    }
      
    def apply(): Array[Byte] = {
      val directoryOutput = new ByteArrayOutputStream()
      val stream = new DataOutputStream(directoryOutput)
      
      for ((name, dict) <- directories) {
        (dict getOrElse ImageDataDirectory(0, 0)).write(stream)
      }
      
      directoryOutput.toByteArray
    }
  }