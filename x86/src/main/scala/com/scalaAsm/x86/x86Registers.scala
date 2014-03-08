package com.scalaAsm.x86

object x86Registers {

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
  
  abstract class Register64(ID: Byte, name: String) extends Register(ID, name) {
    type Size = QwordOperand
  }
  
  class AL extends Register8(0, "al")
  class BL extends Register8(3, "bl")
  class CL extends Register8(1, "cl")
  class DL extends Register8(2, "dl")

  class AH extends Register8(4, "ah")
  class BH extends Register8(7, "bh")
  class CH extends Register8(5, "ch")
  class DH extends Register8(6, "dh")

  // Segment registers
  class ES extends Register8(0, "es")
  class CS extends Register8(1, "cs")
  class SS extends Register8(2, "ss")
  class DS extends Register8(3, "ds")
  class FS extends Register8(4, "fs")
  class GS extends Register8(5, "gs")

  class AX extends Register16(0, "ax")
  class BX extends Register16(3, "bx")
  class CX extends Register16(1, "cx")
  class DX extends Register16(2, "dx")

  class SP extends Register16(4, "sp")
  class BP extends Register16(5, "bp")
  class SI extends Register16(6, "si")
  class DI extends Register16(7, "di")

  // eax - Accumulator for operands and results
  // ebx - Pointer to data in the DS segment
  // ecx - Counter for string and loop operations
  // edx - I/O pointer

  // esp - Stack pointer
  // ebp - Pointer to data on the stack
  // esi - Pointer to data in the segment pointed to by the DS register
  // edi - Pointer to data (or destination) in the segment pointed to by ES register

  class EAX extends Register32(0, "eax")
  class EBX extends Register32(3, "ebx")
  class ECX extends Register32(1, "ecx")
  class EDX extends Register32(2, "edx")

  class ESP extends Register32(4, "esp")
  class EBP extends Register32(5, "ebp")
  class ESI extends Register32(6, "esi")
  class EDI extends Register32(7, "edi")
  
  class RAX extends Register64(0, "rax")
  class RBX extends Register64(3, "rbx")
  class RCX extends Register64(1, "rcx")
  class RDX extends Register64(2, "rdx")

  class RSP extends Register64(4, "rsp")
  class RBP extends Register64(5, "rbp")
  class RSI extends Register64(6, "rsi")
  class RDI extends Register64(7, "rdi")
}