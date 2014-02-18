package com.scalaAsm.x86

object x86Registers {
  import Addressing._

  abstract class Register(val ID: Byte) extends RegisterOrMemory {
    val reg = this
    val isMemory = false
    val offset: Option[Immediate] = None
  }

  abstract class Register8(ID: Byte) extends Register(ID) {
    type Size = ByteOperand
  }
  
  abstract class Register16(ID: Byte) extends Register(ID) {
    type Size = WordOperand
  }
  
  abstract class Register32(ID: Byte) extends Register(ID) {
    type Size = DwordOperand
  }
  
  abstract class r64(ID: Byte) extends Register(ID)

  class AL extends Register8(0) with Addressable[AL]
  class CL extends Register8(1) with Addressable[CL]
  class DL extends Register8(2) with Addressable[DL]
  class BL extends Register8(3) with Addressable[BL]
  class AH extends Register8(4) with Addressable[AH]
  class CH extends Register8(5) with Addressable[CH]
  class DH extends Register8(6) with Addressable[DH]
  class BH extends Register8(7) with Addressable[BH]

  // Segment registers
  class ES extends Register8(0) with Addressable[ES]
  class CS extends Register8(1) with Addressable[CS]
  class SS extends Register8(2) with Addressable[SS]
  class DS extends Register8(3) with Addressable[DS]
  class FS extends Register8(4) with Addressable[FS]
  class GS extends Register8(5) with Addressable[GS]

  class AX extends Register16(0) with Addressable[AX]
  class CX extends Register16(1) with Addressable[CX]
  class DX extends Register16(2) with Addressable[DX]
  class BX extends Register16(3) with Addressable[BX]
  class SP extends Register16(4) with Addressable[SP]
  class BP extends Register16(5) with Addressable[BP]
  class SI extends Register16(6) with Addressable[SI]
  class DI extends Register16(7) with Addressable[DI]

  // Accumulator for operands and results
  class EAX extends Register32(0) with Addressable[EAX]

  // Counter for string and loop operations
  class ECX extends Register32(1) with Addressable[ECX]

  // I/O pointer
  class EDX extends Register32(2) with Addressable[EDX]

  // Pointer to data in the DS segment
  class EBX extends Register32(3) with Addressable[EBX]

  // Stack pointer
  class ESP extends Register32(4) with Addressable[ESP]

  // Pointer to data on the stack
  class EBP extends Register32(5) with Addressable[EBP]

  // Pointer to data in the segment pointed to by the DS register
  class ESI extends Register32(6) with Addressable[ESI]

  // Pointer to data (or destination) in the segment pointed to by ES register
  class EDI extends Register32(7) with Addressable[EDI]
}