package com.scalaAsm.x86

object x86Registers {
  import Addressing._

  abstract class Register(val ID: Byte)

  abstract class r8(ID: Byte) extends Register(ID)
  abstract class r16(ID: Byte) extends Register(ID) with rm16 {
    val reg: Register = this
    val isMemory: Boolean = false
    val offset8: Byte = 0
  }
  abstract class r32(ID: Byte) extends Register(ID) with rm32 {
    val reg: Register = this
    val isMemory: Boolean = false
    val offset8: Byte = 0
  }
  abstract class r64(ID: Byte) extends Register(ID)

  class AL extends r8(0) with Addressable[AL]
  class CL extends r8(1) with Addressable[CL]
  class DL extends r8(2) with Addressable[DL]
  class BL extends r8(3) with Addressable[BL]
  class AH extends r8(4) with Addressable[AH]
  class CH extends r8(5) with Addressable[CH]
  class DH extends r8(6) with Addressable[DH]
  class BH extends r8(7) with Addressable[BH]

  // Segment registers
  class ES extends r8(0) with Addressable[ES]
  class CS extends r8(1) with Addressable[CS]
  class SS extends r8(2) with Addressable[SS]
  class DS extends r8(3) with Addressable[DS]
  class FS extends r8(4) with Addressable[FS]
  class GS extends r8(5) with Addressable[GS]

  class AX extends r16(0) with Addressable[AX]
  class CX extends r16(1) with Addressable[CX]
  class DX extends r16(2) with Addressable[DX]
  class BX extends r16(3) with Addressable[BX]
  class SP extends r16(4) with Addressable[SP]
  class BP extends r16(5) with Addressable[BP]
  class SI extends r16(6) with Addressable[SI]
  class DI extends r16(7) with Addressable[DI]

  // Accumulator for operands and results
  class EAX extends r32(0) with Addressable[EAX]

  // Counter for string and loop operations
  class ECX extends r32(1) with Addressable[ECX]

  // I/O pointer
  class EDX extends r32(2) with Addressable[EDX]

  // Pointer to data in the DS segment
  class EBX extends r32(3) with Addressable[EBX]

  // Stack pointer
  class ESP extends r32(4) with Addressable[ESP]

  // Pointer to data on the stack
  class EBP extends r32(5) with Addressable[EBP]

  // Pointer to data in the segment pointed to by the DS register
  class ESI extends r32(6) with Addressable[ESI]

  // Pointer to data (or destination) in the segment pointed to by ES register
  class EDI extends r32(7) with Addressable[EDI]
}