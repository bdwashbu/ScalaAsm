package com.scalaAsm.x86
package Operands

import com.scalaAsm.x86.Operands.Memory.AddressingMode
import com.scalaAsm.x86.Instructions.`package`.Operand

abstract class Register[S <: OperandSize](val name: String)

abstract class GeneralPurpose[S <: OperandSize](name: String) extends Register[S](name) {
  self =>
    
  val ID: Int
  def -[Z <: Constant[Z]](offset: Operand[Z,Z]) = new BI[Z](offset.get.negate) {}
  def +[Z <: Constant[Z]](offset: Operand[Z,Z]) = new BI[Z](offset.get) {}
  
  abstract class BI[-Disp <: Constant[_]](disp: Disp) extends AddressingMode[S] {
    def base: GPR = self
    def displacement: Constant[_] = disp
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

trait UniformByteRegister[Size <: OperandSize] extends GeneralPurpose[Size]

trait RegisterOperand[X] extends Operand[X,X] {
  self: X =>
  def get = this
}

// "A" family - Accumulator for operands and results
class RAX extends GeneralPurposeA[_64]("rax") with RegisterOperand[RAX]
class EAX extends GeneralPurposeA[_32]("eax") with RegisterOperand[EAX]
class AX extends GeneralPurposeA[_16]("ax") with RegisterOperand[AX]
class AL extends GeneralPurposeA[_8]("al") with RegisterOperand[AL]
class AH extends GeneralPurposeA[_8]("ah")  with RegisterOperand[AH] { override val ID = 4 }

// "B" family - Pointer to data in the DS segment
class RBX extends GeneralPurposeB[_64]("rbx") with RegisterOperand[RBX] 
class EBX extends GeneralPurposeB[_32]("ebx") with RegisterOperand[EBX]
class BX extends GeneralPurposeB[_16]("bx") with RegisterOperand[BX]
class BL extends GeneralPurposeB[_8]("bl") with RegisterOperand[BL]
class BH extends GeneralPurposeB[_8]("bh") with RegisterOperand[BH] { override val ID = 7 }

// "C" family - Counter for string and loop operations
class RCX extends GeneralPurposeC[_64]("rcx") with RegisterOperand[RCX]
class ECX extends GeneralPurposeC[_32]("ecx") with RegisterOperand[ECX]
class CX extends GeneralPurposeC[_16]("cx") with RegisterOperand[CX]
class CL extends GeneralPurposeC[_8]("cl") with RegisterOperand[CL]
class CH extends GeneralPurposeC[_8]("ch") with RegisterOperand[CH] { override val ID = 5 }

// "D" family - I/O pointer
class RDX extends  GeneralPurposeD[_64]("rdx") with RegisterOperand[RDX]
class EDX extends GeneralPurposeD[_32]("edx") with RegisterOperand[EDX]
class DX extends GeneralPurposeD[_16]("dx") with RegisterOperand[DX]
class DL extends GeneralPurposeD[_8]("dl") with RegisterOperand[DL]
class DH extends GeneralPurposeD[_8]("dh") with RegisterOperand[DH] { override val ID = 6 }

class RSP extends StackPointer[_64]("rsp") with RegisterOperand[RSP]
class ESP extends StackPointer[_32]("esp") with RegisterOperand[ESP]
class SP extends StackPointer[_16]("sp") with RegisterOperand[SP]
class SPL extends StackPointer[_8]("spl") with RegisterOperand[SPL] with UniformByteRegister[_8]

class RBP extends BasePointer[_64]("rbp") with RegisterOperand[RBP]
class EBP extends BasePointer[_32]("ebp") with RegisterOperand[EBP]
class BP extends BasePointer[_16]("bp") with RegisterOperand[BP]

class RSI extends SourceIndex[_64]("rsi") with RegisterOperand[RSI]
class ESI extends SourceIndex[_32]("esi") with RegisterOperand[ESI]
class SI extends SourceIndex[_16]("si") with RegisterOperand[SI]

class RDI extends DestinationIndex[_64]("rdi") with RegisterOperand[RDI]
class EDI extends DestinationIndex[_32]("edi") with RegisterOperand[EDI]
class DI extends DestinationIndex("di") with RegisterOperand[DI]

class ES extends SegmentRegister("es") with RegisterOperand[ES] { val ID = 8 }
class CS extends SegmentRegister("cs") with RegisterOperand[CS] { val ID = 8 }
class SS extends SegmentRegister("ss") with RegisterOperand[SS] { val ID = 8 }
class DS extends SegmentRegister("ds") with RegisterOperand[DS] { val ID = 8 }
class FS extends SegmentRegister("fs") with RegisterOperand[FS] { val ID = 8 }
class GS extends SegmentRegister("gs") with RegisterOperand[GS] { val ID = 8 }

// Extra 64-bit registers
class R8 extends GeneralPurpose[_64]("r8") with RegisterOperand[R8] { val ID = 8 }
class R9 extends GeneralPurpose[_64]("r9") with RegisterOperand[R9] { val ID = 9 }
class R10 extends GeneralPurpose[_64]("r10") with RegisterOperand[R10] { val ID = 10 }
class R11 extends GeneralPurpose[_64]("r11") with RegisterOperand[R11] { val ID = 11 }
class R12 extends GeneralPurpose[_64]("r12") with RegisterOperand[R12] { val ID = 12 }
class R13 extends GeneralPurpose[_64]("r13") with RegisterOperand[R13] { val ID = 13 }
class R14 extends GeneralPurpose[_64]("r14") with RegisterOperand[R14] { val ID = 14 }
class R15 extends GeneralPurpose[_64]("r15") with RegisterOperand[R15] { val ID = 15 }