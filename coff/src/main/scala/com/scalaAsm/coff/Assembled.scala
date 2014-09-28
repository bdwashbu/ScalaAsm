package com.scalaAsm.coff

import com.scalaAsm.portableExe.CompiledImports
import java.io.File
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import com.scalaAsm.portableExe.DosHeader
import com.scalaAsm.portableExe.PeHeader
import com.scalaAsm.portableExe.DataDirectories
import com.scalaAsm.portableExe.sections.Sections
import com.scalaAsm.portableExe.sections.ImageExportDirectory
import com.scalaAsm.portableExe.sections.Extern
import com.scalaAsm.portableExe.sections.Imports
import com.scalaAsm.portableExe.PortableExecutable
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.portableExe.sections.ResourceGen


abstract class Assembled(val codeTokens: Seq[Any], val dataTokens: Seq[Token], val iconPath: Option[String] = None) {
  self =>
  val rawData: Array[Byte]
  val variables: (Int) => Map[String, Int]
  val unboundSymbols: Seq[String]

  case class CompiledAssembly(onePass: Seq[Token], positionPass: Seq[PostToken])
  
  def finalizeAssembly(variables: Map[String, Int], imports: Map[String, Int], imports64: Map[String, Int], baseOffset: Int): Array[Byte]

  def addIcon(path: String): Assembled = {
    new Assembled(codeTokens, dataTokens, Option(path)) {
      val rawData = self.rawData
      val variables = self.variables
      val unboundSymbols = self.unboundSymbols

      def finalizeAssembly(variables: Map[String, Int], imports: Map[String, Int], imports64: Map[String, Int], baseOffset: Int): Array[Byte] = {
        self.finalizeAssembly(variables, imports, imports64, baseOffset)
      }
    }
  }
}