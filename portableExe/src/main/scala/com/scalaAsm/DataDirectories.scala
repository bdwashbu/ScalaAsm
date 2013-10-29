package com.scalaAsm.portableExe

import java.io.DataOutputStream
import scala.collection.immutable.TreeMap
import java.io.ByteArrayOutputStream

case class Directory(val virtualAddress: Int, val size: Int) extends ExeWriter {
  def write(stream: DataOutputStream) {
    write(stream, virtualAddress)
    write(stream, size)
  }
}

case class DataDirectories(val imports: Directory, val importAddressTable: Directory) extends Function0[Array[Byte]]{
  
    object DirectoryTypes extends Enumeration {
      type characteristic = Value
      val Export, Import, Resource, Exception, Security, BaseReloc, Debug, Copyright,
            GlobalPtr, TLS, LoadConfig, BoundImport, IAT, COM, DelayImport, Reserved = Value
    }

    import DirectoryTypes._
    
    // Virtual Address and Size
    val directories = TreeMap(
      Export -> None,
      Import -> Some(imports),
      Resource -> None,
      Exception -> None,
      Security -> None,
      BaseReloc -> None,
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

    def apply(): Array[Byte] = {
      val directoryOutput = new ByteArrayOutputStream()
      val stream = new DataOutputStream(directoryOutput)
      
      for ((name, dict) <- directories) {
        (dict getOrElse Directory(0, 0)).write(stream)
      }
      
      directoryOutput.toByteArray
    }
  }