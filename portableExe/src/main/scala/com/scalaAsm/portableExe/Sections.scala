package com.scalaAsm.portableExe

private[portableExe] case class CompiledSections(sectionHeaders: SectionHeader*) {
  val sections = sectionHeaders.map(_.write).reduce(_ ++ _)
}

private[portableExe] trait Sections {

  def compileSections(codeSize: Int, dataSize: Int): CompiledSections = {

    val textSection = SectionHeader(name = ".text",
      virtualSize = codeSize,
      virtualAddress = 0x1000,
      sizeOfRawData = 0x200,
      pointerOrRawData = 0x200,
      relocPtr = 0,
      linenumPtr = 0,
      relocations = 0,
      lineNumbers = 0,
      characteristics = Characteristic.CODE.id |
        Characteristic.EXECUTE.id |
        Characteristic.READ.id)

    val dataSection = SectionHeader(name = ".data",
      virtualSize = dataSize,
      virtualAddress = 0x2000,
      sizeOfRawData = 0x200,
      pointerOrRawData = 0x400,
      relocPtr = 0,
      linenumPtr = 0,
      relocations = 0,
      lineNumbers = 0,
      characteristics = Characteristic.INITIALIZED.id |
        Characteristic.READ.id |
        Characteristic.WRITE.id)

    CompiledSections(textSection, dataSection)
  }
}