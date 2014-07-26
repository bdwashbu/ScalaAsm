package com.scalaAsm.x86
package Operands

import com.scalaAsm.x86.Operands.Memory._
import com.scalaAsm.x86.Operands.Memory.WithSIBWithDisplacement
import com.scalaAsm.x86.Operands.Memory.TwoRegisters
import com.scalaAsm.x86.Operands.Memory.SIB
import com.scalaAsm.x86.Operands.Memory.NoSIBWithDisplacement
import com.scalaAsm.x86.Operands.Memory.ModRMReg
import com.scalaAsm.x86.Operands.Memory.ModRMOpcode
import com.scalaAsm.x86.Operands.Memory.DisplacementDword
import com.scalaAsm.x86.Operands.Memory.DisplacementByte
import com.scalaAsm.x86.Operands.Memory.AddressingFormSpecifier
import com.scalaAsm.x86.Operands.Memory.OnlyModRM

//   case class RegisterOffset[S <: Constant[S], +T <: GPR](offset2: S, reg: T) extends BaseIndex {
//     type Size = DwordOperand
//     val base = reg
//     val displacement = offset2
//  }

  abstract class Register(val name: String)

  abstract class Register8(name: String) extends Register(name) {
    type Size = ByteOperand
    def size = 1
  }
  
  abstract class Register16(name: String) extends Register(name) {
    type Size = WordOperand
    def size = 2
  }
  
  abstract class Register32(name: String) extends Register(name) {
    type Size = DwordOperand
    def size = 4
  }
  
  abstract class Register64(name: String) extends Register(name) {
    type Size = QwordOperand
    def size = 8
  }

  trait GeneralPurpose[Self] extends RegisterOrMemory {
    self:Register => 
      val ID: Int
      def -[Z <: Constant[Z]](offset: Z) = new BaseIndex[GeneralPurpose[Self], Z](this, offset.negate) {type Size = self.Size}
      def +[Z <: Constant[Z]](offset: Z) = new BaseIndex[GeneralPurpose[Self], Z](this, offset) {type Size = self.Size}
      override def toString = name
//      
//     def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
//	    OnlyModRM(ModRMOpcode(TwoRegisters, opcodeExtend.get, this))
//	 }
//      
//     def encode(op2: BaseIndex, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
//	    (op2.base, op2.offset) match {
//	      case (base, off: Displacement8) if base.ID == 4 =>
//	        WithSIBWithDisplacement(ModRMReg(DisplacementByte, this, base), SIB(SIB.One, new ESP, base), op2.offset)
//	      case (base, off: Displacement32) =>
//	        NoSIBWithDisplacement(ModRMReg(DisplacementDword, reg = this, rm = base), op2.offset)
//	      case (base, _: Displacement) =>
//	        NoSIBWithDisplacement(ModRMReg(DisplacementByte, reg = this, rm = base), op2.offset)
//	      //case (base, None) =>
//	       // NoSIB(ModRMReg(NoDisplacement, reg = this, rm = base))
//	    }
//	  }
//     
//     def encode(op2: GPR, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
//	    OnlyModRM(ModRMReg(TwoRegisters, this, op2))
//	 }
  }
  
  trait GeneralPurposeA[Self] extends GeneralPurpose[Self] {self:Register => val ID = 0}
  trait GeneralPurposeB[Self] extends GeneralPurpose[Self] {self:Register => val ID = 3}
  trait GeneralPurposeC[Self] extends GeneralPurpose[Self] {self:Register => val ID = 1}
  trait GeneralPurposeD[Self] extends GeneralPurpose[Self] {self:Register => val ID = 2}
  
  trait SourceIndex[Self] extends GeneralPurpose[Self] {self:Register => val ID = 6}
  trait DestinationIndex[Self] extends GeneralPurpose[Self] {self:Register => val ID = 7}
  trait BasePointer[Self] extends GeneralPurpose[Self] {self:Register => val ID = 5}
  trait StackPointer[Self] extends GeneralPurpose[Self] {self:Register => val ID = 4}

  trait UniformByteRegister[Self] extends GeneralPurpose[Self] {
    self:Register => 
  }
  
  // "A" family - Accumulator for operands and results
  class RAX extends Register64("rax") with GeneralPurposeA[RAX]
  class EAX extends Register32("eax") with GeneralPurposeA[EAX]
  class AX extends Register16("ax") with GeneralPurposeA[AX]
  class AL extends Register8("al") with GeneralPurposeA[AL]
  class AH extends Register8("ah") with GeneralPurposeA [AH] {override val ID = 4}
  
  // "B" family - Pointer to data in the DS segment
  class RBX extends Register64("rbx") with GeneralPurposeB[RBX]
  class EBX extends Register32("ebx") with GeneralPurposeB[EBX]
  class BX extends Register16("bx") with GeneralPurposeB[BX]
  class BL extends Register8("bl") with GeneralPurposeB[BL]
  class BH extends Register8("bh") with GeneralPurposeB[BH] {override val ID = 7}
  
  // "C" family - Counter for string and loop operations
  class RCX extends Register64("rcx") with GeneralPurposeC[RCX]
  class ECX extends Register32("ecx") with GeneralPurposeC[ECX]
  class CX extends Register16("cx") with GeneralPurposeC[CX]
  class CL extends Register8("cl") with GeneralPurposeC[CL]
  class CH extends Register8("ch") with GeneralPurposeC[CH] {override val ID = 5}

  // "D" family - I/O pointer
  class RDX extends Register64("rdx") with GeneralPurposeD[RDX]
  class EDX extends Register32("edx") with GeneralPurposeD[EDX]
  class DX extends Register16("dx") with GeneralPurposeD[DX]
  class DL extends Register8("dl") with GeneralPurposeD[DL]
  class DH extends Register8("dh") with GeneralPurposeD[DH] {override val ID = 6}

  class RSP extends Register64("rsp") with StackPointer[RSP]
  class ESP extends Register32("esp") with StackPointer[ESP]
  class SP extends Register16("sp") with StackPointer[SP]
  class SPL extends Register8("spl") with StackPointer[SPL] with UniformByteRegister[SPL]
  
  class RBP extends Register64("rbp") with BasePointer[RBP]
  class EBP extends Register32("ebp") with BasePointer[EBP]
  class BP extends Register16("bp") with BasePointer[BP]
  
  class RSI extends Register64("rsi") with SourceIndex[RSI]
  class ESI extends Register32("esi") with SourceIndex[ESI]
  class SI extends Register16("si") with SourceIndex[SI]
  
  class RDI extends Register64("rdi") with DestinationIndex[RDI]
  class EDI extends Register32("edi") with DestinationIndex[EDI]
  class DI extends Register16("di") with DestinationIndex [DI]
  
  class ES extends SegmentRegister("es") with Operand
  class CS extends SegmentRegister("cs") with Operand
  class SS extends SegmentRegister("ss") with Operand
  class DS extends SegmentRegister("ds") with Operand
  class FS extends SegmentRegister("fs") with Operand
  class GS extends SegmentRegister("gs") with Operand
  
  // Extra 64-bit registers
  class R8 extends Register64("r8") with GeneralPurpose[R8] {self:Register => val ID = 8}
  class R9 extends Register64("r9") with GeneralPurpose[R9] {self:Register => val ID = 9}
  class R10 extends Register64("r10") with GeneralPurpose[R10] {self:Register => val ID = 10}
  class R11 extends Register64("r11") with GeneralPurpose[R11] {self:Register => val ID = 11}
  class R12 extends Register64("r12") with GeneralPurpose[R12] {self:Register => val ID = 12}
  class R13 extends Register64("r13") with GeneralPurpose[R13] {self:Register => val ID = 13}
  class R14 extends Register64("r14") with GeneralPurpose[R14] {self:Register => val ID = 14}
  class R15 extends Register64("r15") with GeneralPurpose[R15] {self:Register => val ID = 15}