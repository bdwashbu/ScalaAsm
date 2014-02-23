package com.scalaAsm.x86

object x86Registers {
  import Addressing._

  abstract class Register(val ID: Byte, val name: String) extends RegisterOrMemory {
    val reg = this
    val isMemory = false
    val offset: Option[Immediate] = None
    override def toString = name
  }

  abstract class Register8(ID: Byte, name: String) extends Register(ID, name) {
    type Size = ByteOperand
  }
  
  abstract class Register16(ID: Byte, name: String) extends Register(ID, name) {
    type Size = WordOperand
  }
  
  abstract class Register32(ID: Byte, name: String) extends Register(ID, name) {
    type Size = DwordOperand
  }
  
  abstract class r64(ID: Byte, name: String) extends Register(ID, name)

  class AL extends Register8(0, "al") with Addressable[AL]
  class CL extends Register8(1, "cl") with Addressable[CL]
  class DL extends Register8(2, "dl") with Addressable[DL]
  class BL extends Register8(3, "bl") with Addressable[BL]
  class AH extends Register8(4, "ah") with Addressable[AH]
  class CH extends Register8(5, "ch") with Addressable[CH]
  class DH extends Register8(6, "dh") with Addressable[DH]
  class BH extends Register8(7, "bh") with Addressable[BH]

  // Segment registers
  class ES extends Register8(0, "es") with Addressable[ES]
  class CS extends Register8(1, "cs") with Addressable[CS]
  class SS extends Register8(2, "ss") with Addressable[SS]
  class DS extends Register8(3, "ds") with Addressable[DS]
  class FS extends Register8(4, "fs") with Addressable[FS]
  class GS extends Register8(5, "gs") with Addressable[GS]

  class AX extends Register16(0, "ax") with Addressable[AX]
  class CX extends Register16(1, "cx") with Addressable[CX]
  class DX extends Register16(2, "dx") with Addressable[DX]
  class BX extends Register16(3, "bx") with Addressable[BX]
  class SP extends Register16(4, "sp") with Addressable[SP]
  class BP extends Register16(5, "bp") with Addressable[BP]
  class SI extends Register16(6, "si") with Addressable[SI]
  class DI extends Register16(7, "di") with Addressable[DI]

  // Accumulator for operands and results
  class EAX extends Register32(0, "eax") with Addressable[EAX]

  // Counter for string and loop operations
  class ECX extends Register32(1, "ecx") with Addressable[ECX]

  // I/O pointer
  class EDX extends Register32(2, "edx") with Addressable[EDX]

  // Pointer to data in the DS segment
  class EBX extends Register32(3, "ebx") with Addressable[EBX]

  // Stack pointer
  class ESP extends Register32(4, "esp") with Addressable[ESP]

  // Pointer to data on the stack
  class EBP extends Register32(5, "ebp") with Addressable[EBP]

  // Pointer to data in the segment pointed to by the DS register
  class ESI extends Register32(6, "esi") with Addressable[ESI]

  // Pointer to data (or destination) in the segment pointed to by ES register
  class EDI extends Register32(7, "edi") with Addressable[EDI]
}