package com.scalaAsm.x86
package Operands

  abstract class Register(val name: String)

  abstract class Register8(name: String) extends Register(name) {
    type Size = ByteOperand
  }
  
  abstract class Register16(name: String) extends Register(name) {
    type Size = WordOperand
  }
  
  abstract class Register32(name: String) extends Register(name) {
    type Size = DwordOperand
  }
  
  abstract class Register64(name: String) extends Register(name) {
    type Size = QwordOperand
  }

  trait GeneralPurpose extends RegisterOrMemory {
    self:Register => 
      val ID: Int
      override def toString = name
  }
  
  trait GeneralPurposeA extends GeneralPurpose {self:Register => val ID = 0}
  trait GeneralPurposeB extends GeneralPurpose {self:Register => val ID = 3}
  trait GeneralPurposeC extends GeneralPurpose {self:Register => val ID = 1}
  trait GeneralPurposeD extends GeneralPurpose {self:Register => val ID = 2}
  
  trait SourceIndex extends GeneralPurpose {self:Register => val ID = 6}
  trait DestinationIndex extends GeneralPurpose {self:Register => val ID = 7}
  trait BasePointer extends GeneralPurpose {self:Register => val ID = 5}
  trait StackPointer extends GeneralPurpose {self:Register => val ID = 4}

  // "A" family - Accumulator for operands and results
  class RAX extends Register64("rax") with GeneralPurposeA
  class EAX extends Register32("eax") with GeneralPurposeA
  class AX extends Register16("ax") with GeneralPurposeA
  class AL extends Register8("al") with GeneralPurposeA
  class AH extends Register8("ah") with GeneralPurposeA {override val ID = 4}
  
  // "B" family - Pointer to data in the DS segment
  class RBX extends Register64("rbx") with GeneralPurposeB
  class EBX extends Register32("ebx") with GeneralPurposeB
  class BX extends Register16("bx") with GeneralPurposeB
  class BL extends Register8("bl") with GeneralPurposeB
  class BH extends Register8("bh") with GeneralPurposeB {override val ID = 7}
  
  // "C" family - Counter for string and loop operations
  class RCX extends Register64("rcx") with GeneralPurposeC
  class ECX extends Register32("ecx") with GeneralPurposeC
  class CX extends Register16("cx") with GeneralPurposeC
  class CL extends Register8("cl") with GeneralPurposeC
  class CH extends Register8("ch") with GeneralPurposeC {override val ID = 5}

  // "D" family - I/O pointer
  class RDX extends Register64("rdx") with GeneralPurposeD
  class EDX extends Register32("edx") with GeneralPurposeD
  class DX extends Register16("dx") with GeneralPurposeD
  class DL extends Register8("dl") with GeneralPurposeD
  class DH extends Register8("dh") with GeneralPurposeD {override val ID = 6}

  class RSP extends Register64("rsp") with StackPointer
  class ESP extends Register32("esp") with StackPointer
  class SP extends Register16("sp") with StackPointer
  
  class RBP extends Register64("rbp") with BasePointer
  class EBP extends Register32("ebp") with BasePointer
  class BP extends Register16("bp") with BasePointer
  
  class RSI extends Register64("rsi") with SourceIndex
  class ESI extends Register32("esi") with SourceIndex
  class SI extends Register16("si") with SourceIndex
  
  class RDI extends Register64("rdi") with DestinationIndex
  class EDI extends Register32("edi") with DestinationIndex
  class DI extends Register16("di") with DestinationIndex 
  
  class ES extends SegmentRegister("es")
  class CS extends SegmentRegister("cs")
  class SS extends SegmentRegister("ss")
  class DS extends SegmentRegister("ds")
  class FS extends SegmentRegister("fs")
  class GS extends SegmentRegister("gs")