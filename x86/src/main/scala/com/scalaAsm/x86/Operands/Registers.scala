package com.scalaAsm.x86
package Operands

import com.scalaAsm.x86.Operands.Memory.AddressingMode

abstract class Register[S <: OperandSize](val name: String)

abstract class GeneralPurpose[S <: OperandSize](name: String) extends Register[S](name) {
  self2 =>
    
  val ID: Int
  def -[Z <: OperandSize](offset: Operand[_,Constant[Z]]) = new BaseIndex(offset.get.negate) {}
  def +[Z <: OperandSize](offset: Operand[_,Constant[Z]]) = new BaseIndex(offset.get) {}
  
  abstract class BaseIndex[Y <: OperandSize](val displacement: Constant[Y]) extends AddressingMode[S] {
    def base: GeneralPurpose[S] = self2
    def get: BaseIndex[Y] = this
  }
  
  abstract class Indirect extends AddressingMode[S] {
    self =>
    def base: GeneralPurpose[S] = self2
    def get = this
  //  def encode(reg: GPR, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
  //    OnlyModRM(ModRMReg(NoDisplacement, reg, rm = base))
  //  }
    
  //  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
  //    OnlyModRM(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
  //  }
  }
}

abstract class GeneralPurposeA[Size <: OperandSize](name: String) extends GeneralPurpose[Size](name) with RegisterOrMemory[Size] { val ID = 0 } 
abstract class GeneralPurposeB[Size <: OperandSize](name: String) extends GeneralPurpose[Size](name) with RegisterOrMemory[Size] { val ID = 3 }
abstract class GeneralPurposeC[Size <: OperandSize](name: String) extends GeneralPurpose[Size](name) with RegisterOrMemory[Size] { val ID = 1 }
abstract class GeneralPurposeD[Size <: OperandSize](name: String) extends GeneralPurpose[Size](name) with RegisterOrMemory[Size] { val ID = 2 }

abstract class SourceIndex[Size <: OperandSize](name: String) extends GeneralPurpose[Size](name) with RegisterOrMemory[Size] { val ID = 6 }
abstract class DestinationIndex[Size <: OperandSize](name: String) extends GeneralPurpose[Size](name) with RegisterOrMemory[Size] { val ID = 7 }
abstract class BasePointer[Size <: OperandSize](name: String) extends GeneralPurpose[Size](name) with RegisterOrMemory[Size] { val ID = 5 }
abstract class StackPointer[Size <: OperandSize](name: String) extends GeneralPurpose[Size](name) with RegisterOrMemory[Size] { val ID = 4 }

abstract class Extra64Reg(name: String) extends GeneralPurpose[_64](name) with RegisterOrMemory[_64]

trait UniformByteRegister[Size <: OperandSize] extends GeneralPurpose[Size] with RegisterOrMemory[Size]

// "A" family - Accumulator for operands and results
class RAX extends GeneralPurposeA[_64]("rax")
class EAX extends GeneralPurposeA[_32]("eax")
class AX extends GeneralPurposeA[_16]("ax")
class AL extends GeneralPurposeA[_8]("al")
class AH extends GeneralPurposeA[_8]("ah") { override val ID = 4 }

// "B" family - Pointer to data in the DS segment
class RBX extends GeneralPurposeB[_64]("rbx")
class EBX extends GeneralPurposeB[_32]("ebx")
class BX extends GeneralPurposeB[_16]("bx")
class BL extends GeneralPurposeB[_8]("bl")
class BH extends GeneralPurposeB[_8]("bh") { override val ID = 7 }

// "C" family - Counter for string and loop operations
class RCX extends GeneralPurposeC[_64]("rcx")
class ECX extends GeneralPurposeC[_32]("ecx")
class CX extends GeneralPurposeC[_16]("cx")
class CL extends GeneralPurposeC[_8]("cl")
class CH extends GeneralPurposeC[_8]("ch") { override val ID = 5 }

// "D" family - I/O pointer
class RDX extends  GeneralPurposeD[_64]("rdx")
class EDX extends GeneralPurposeD[_32]("edx")
class DX extends GeneralPurposeD[_16]("dx")
class DL extends GeneralPurposeD[_8]("dl")
class DH extends GeneralPurposeD[_8]("dh") { override val ID = 6 }

class RSP extends StackPointer[_64]("rsp")
class ESP extends StackPointer[_32]("esp")
class SP extends StackPointer[_16]("sp")
class SPL extends StackPointer[_8]("spl") with UniformByteRegister[_8]

class RBP extends BasePointer[_64]("rbp")
class EBP extends BasePointer[_32]("ebp")
class BP extends BasePointer[_16]("bp")

class RSI extends SourceIndex[_64]("rsi")
class ESI extends SourceIndex[_32]("esi")
class SI extends SourceIndex[_16]("si")

class RDI extends DestinationIndex[_64]("rdi")
class EDI extends DestinationIndex[_32]("edi")
class DI extends DestinationIndex("di")

class ES extends SegmentRegister("es") { val ID = 8 }
class CS extends SegmentRegister("cs") { val ID = 8 }
class SS extends SegmentRegister("ss") { val ID = 8 }
class DS extends SegmentRegister("ds") { val ID = 8 }
class FS extends SegmentRegister("fs") { val ID = 8 }
class GS extends SegmentRegister("gs") { val ID = 8 }

// Extra 64-bit registers
// Uses the rex.W field to access
class R8 extends Extra64Reg("r8") { val ID = 0 }
class R9 extends Extra64Reg("r9") { val ID = 1 }
class R10 extends Extra64Reg("r10") { val ID = 2 }
class R11 extends Extra64Reg("r11") { val ID = 3 }
class R12 extends Extra64Reg("r12") { val ID = 4 }
class R13 extends Extra64Reg("r13") { val ID = 5 }
class R14 extends Extra64Reg("r14") { val ID = 6 }
class R15 extends Extra64Reg("r15") { val ID = 7 }