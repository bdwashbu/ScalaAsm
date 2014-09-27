package com.scalaAsm.portableExe

import java.io.{ OutputStream }
import java.nio.{ ByteBuffer, ByteOrder }
import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeMap
import java.io.DataOutputStream
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import java.io.File
import java.io.FileInputStream
import sections._
import com.scalaAsm.portableExe._
  
case class CompiledImports(rawData: Array[Byte],
                            boundImportSize: Int,
                            nameTableSize: Int,
                            imports: Map[String, Int],
                            importSymbols: Seq[ImportSymbol]) {
  
  def getImportsDirectory(importsLocation: Int) = {
    ImageDataDirectory(importsLocation, boundImportSize)
  }
  
  def getIATDirectory(IATlocation: Int) = {
    ImageDataDirectory(IATlocation, nameTableSize)
  }  
}

