package com.scalaAsm.portableExe

import java.io.DataOutputStream
import scala.collection.immutable.TreeMap
import java.io.ByteArrayOutputStream

private[portableExe] case class ImageDataDirectory(virtualAddress: Int, size: Int) extends ExeWriter {
  def write(stream: DataOutputStream) {
    write(stream, virtualAddress)
    write(stream, size)
  }
}

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