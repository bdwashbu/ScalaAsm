package com.scalaAsm.coff

abstract class Assembled(val iconPath: Option[String] = None) {
  self =>
  val rawData: Array[Byte]
  val variables: (Int) => Map[String, Int]
  val unboundSymbols: Seq[String]

  def finalizeAssembly(addressOfData: Int, imports: Map[String, Int], imports64: Map[String, Int], baseOffset: Int): Array[Byte]

  def addIcon(path: String): Assembled = {
    new Assembled(Option(path)) {
      val rawData = self.rawData
      val variables = self.variables
      val unboundSymbols = self.unboundSymbols

      def finalizeAssembly(addressOfData: Int, imports: Map[String, Int], imports64: Map[String, Int], baseOffset: Int): Array[Byte] = {
        self.finalizeAssembly(addressOfData, imports, imports64, baseOffset)
      }
    }
  }
}