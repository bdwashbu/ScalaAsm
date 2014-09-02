package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._
import com.scalaAsm.x86.OneOpcode
import com.scalaAsm.x86.OpcodeFormat
import com.scalaAsm.x86.REX
import com.scalaAsm.x86.TwoOpcodes
import scala.language.implicitConversions
import com.scalaAsm.x86.Operands._

trait LowPriorityFormats extends OperandEncoding {

  implicit object MFormat extends OneOperandFormat[M, rel] {

    def apply(operand1Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedOneOperand[rel](operand1Size, opcode, prefix) {
      def getAddressingForm(operand: rel) = {
  
            InstructionFormat (
              addressingForm = OnlyDisplacement(operand.displacement),
              immediate = None
            )
          
      }
      
       def size = prefix.size + opcode.size + operand1Size
    }
  }
  
  implicit object MFormat2 extends OneOperandFormat[M, RegisterIndirect[_32]] {

    def apply(operand1Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedOneOperand[RegisterIndirect[_32]](operand1Size, opcode, prefix) {
      def getAddressingForm(operand: RegisterIndirect[_32]) = {
            InstructionFormat (
              addressingForm = OnlyModRM(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, operand.base)), //mem.encode(opcode.opcodeExtension),
              immediate = None
            )
      }
      
       def size = prefix.size + opcode.size + 1
    }
  }
  
  implicit object MFormat2R2 extends OneOperandFormat[M, RegisterIndirect[_64]] {

    def apply(operand1Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedOneOperand[RegisterIndirect[_64]](operand1Size, opcode, prefix) {
      def getAddressingForm(operand: RegisterIndirect[_64]) = {
            InstructionFormat (
              addressingForm = OnlyModRM(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, operand.base)), //mem.encode(opcode.opcodeExtension),
              immediate = None
            )
      }
      
       def size = prefix.size + opcode.size + 1
    }
  }
  
  
  
  implicit object MFormatB2 extends OneOperandFormat[M, BaseIndex[_,Constant8]] {

    def apply(operand1Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedOneOperand[BaseIndex[_,Constant8]](operand1Size, opcode, prefix) {
      def getAddressingForm(operand: BaseIndex[_,Constant8]) = {
            InstructionFormat (
              NoSIBWithDisplacement(ModRMOpcode(DisplacementByte, opcode.opcodeExtension.get, operand.base), operand.displacement),
              immediate = None
            )
      }
      
       def size = prefix.size + opcode.size + 1 + operand1Size
    }
  }
  
  implicit object MFormat4 extends OneOperandFormat[M, GPR] {

    def apply(operand1Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedOneOperand[GPR](operand1Size, opcode, prefix) {
      def getAddressingForm(operand: GPR) = {
            InstructionFormat (
              addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcode.opcodeExtension.get, operand)),///reg.encode(opcode.opcodeExtension),
              immediate = None
            )
      }
      
       def size = prefix.size + opcode.size + 1
    }
  }

  implicit object DSFormat extends OneOperandFormat[DS, DS] {
    def apply(operand1Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedOneOperand[DS](operand1Size, opcode, prefix) {
      def getAddressingForm(op1: DS) = NoAddressingForm
      def size = prefix.size + opcode.size
    }
  }

  implicit object CSFormat extends OneOperandFormat[CS, CS] {
    def apply(operand1Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedOneOperand[CS](operand1Size, opcode, prefix) {
      def getAddressingForm(op1: CS) = NoAddressingForm
      def size = prefix.size + opcode.size
    }
  }

  implicit object OFormat extends OneOperandFormat[O, ModRM.plusRd] {
    def apply(operand1Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedOneOperand[ModRM.plusRd](operand1Size, opcode, prefix) {
      def getAddressingForm(operand: ModRM.plusRd) = {
        InstructionFormat (
          addressingForm = NoModRM(),
          immediate = None
        )
      }
      
      def size = prefix.size + opcode.size
    }
  }

  implicit object IFormat extends OneOperandFormat[I, imm] {

    def apply(operand1Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedOneOperand[imm](operand1Size, opcode, prefix) {
      def getAddressingForm(operand: imm) = {
        InstructionFormat (
          addressingForm = NoModRM(),
          immediate = Some(operand)
        )
      }
      
      def size = prefix.size + opcode.size + operand1Size
    }
  }

  implicit object OffsetFormat extends OneOperandFormat[Offset, BaseIndex[r64,_]] {

    def apply(operand1Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedOneOperand[BaseIndex[r64,_]](operand1Size, opcode, prefix) {
      def getAddressingForm(operand: BaseIndex[r64,_]) = {
        InstructionFormat (
          WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, operand.base), ScaleIndexByte(SIB.One, new ESP, operand.base)),
          immediate = None
        )
      }
      
      def size = prefix.size + opcode.size + 2
    }
  }
  
  implicit object OffsetFormat2 extends OneOperandFormat[Offset, BaseIndex[_,Constant8]] {

    def apply(operand1Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedOneOperand[BaseIndex[_,Constant8]](operand1Size, opcode, prefix) {
      def getAddressingForm(operand: BaseIndex[_,Constant8]) = {
        InstructionFormat (
          NoSIBWithDisplacement(ModRMOpcode(DisplacementByte, opcode.opcodeExtension.get, operand.base), operand.displacement),
          immediate = None
        )
      }
      
      def size = prefix.size + opcode.size + 1 + operand1Size
    }
  }
  
  implicit object MRFormat extends TwoOperandFormat[MR, BaseIndex[_,Constant8], ModRM.reg] {

    def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedTwoOperands[BaseIndex[_,Constant8], ModRM.reg](operand1Size,operand2Size,opcode, prefix) {
      def getAddressingForm(op1: BaseIndex[_,Constant8], op2: ModRM.reg) = {
        RMFormat(operand2Size, operand1Size, opcode, prefix).getAddressingForm(op2, op1)
      }
  
      def size = RMFormat(operand2Size, operand1Size, opcode, prefix).size
    }
  }
  
  implicit object RMFormat extends TwoOperandFormat[RM, ModRM.reg, BaseIndex[_,Constant8]] {
    
    def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedTwoOperands[ModRM.reg, BaseIndex[_,Constant8]](operand1Size,operand2Size,opcode, prefix) {
      def getAddressingForm(op1: ModRM.reg, op2: BaseIndex[_,Constant8]) = {
        InstructionFormat (
            NoSIBWithDisplacement(ModRMReg(DisplacementByte, reg = op1, rm = op2.base), op2.displacement),
            immediate = None
        )
      }
  
      def size = prefix.size + opcode.size + 1 + operand2Size
    }
  }
  
  implicit object MIFormat extends TwoOperandFormat[MI, ModRM.rm, imm] {

    def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedTwoOperands[ModRM.rm, imm](operand1Size,operand2Size, opcode, prefix) {
      def getAddressingForm(op1: ModRM.rm, op2: imm) = {
  
        op1 match {
          case reg: GPR =>
            InstructionFormat (
              addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcode.opcodeExtension.get, reg)),//reg.encode(opcode.opcodeExtension),
              immediate = Some(op2)
            )
        }
      }
  
      def size = prefix.size + opcode.size + 1 + operand2Size
    }
  }
  
  implicit object OIFormat extends TwoOperandFormat[OI, ModRM.plusRd, imm] {

    def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedTwoOperands[ModRM.plusRd, imm](operand1Size,operand2Size,opcode, prefix) {
      def getAddressingForm(op1: ModRM.plusRd, op2: imm) = {
        InstructionFormat (
          addressingForm = NoModRM(),
          immediate = Some(op2)
        )
      }
  
      def size = prefix.size + opcode.size + operand2Size
    }
  }
}



trait Formats extends LowPriorityFormats {
  
  implicit object MFormatB1 extends OneOperandFormat[M, BaseIndex[r64, _]] {

    def apply(operand1Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedOneOperand[BaseIndex[r64, _]](operand1Size, opcode, prefix) {
      def getAddressingForm(operand: BaseIndex[r64, _]) = {
            InstructionFormat (
              WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, operand.base), ScaleIndexByte(SIB.One, new ESP, operand.base)),
              immediate = None
            )
      }
      
       def size = prefix.size + opcode.size + 2
    }
  }
  
  implicit object RMFormatB1 extends TwoOperandFormat[RM, ModRM.reg, BaseIndex[StackPointer[_],Constant8]] {

    def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedTwoOperands[ModRM.reg, BaseIndex[StackPointer[_],Constant8]](operand1Size,operand2Size,opcode,prefix) {
      def getAddressingForm(op1: ModRM.reg, op2: BaseIndex[StackPointer[_],Constant8]) = {
        InstructionFormat (
      	    WithSIBWithDisplacement(ModRMReg(DisplacementByte, op1, op2.base), ScaleIndexByte(SIB.One, new ESP, op2.base), op2.displacement),
            immediate = None
        )
      }
  
      def size = prefix.size + opcode.size + 2 + operand2Size
    }
  }
  
  implicit object RMFormatB2 extends TwoOperandFormat[RM, ModRM.reg, BaseIndex[_,Constant32]] {

    def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedTwoOperands[ModRM.reg, BaseIndex[_,Constant32]](operand1Size,operand2Size,opcode,prefix) {
      def getAddressingForm(op1: ModRM.reg, op2: BaseIndex[_,Constant32]) = {
        InstructionFormat (
      	    NoSIBWithDisplacement(ModRMReg(DisplacementDword, reg = op1, rm = op2.base), op2.displacement),  
            immediate = None
        )
      }
  
      def size = prefix.size + opcode.size + 1 + operand2Size
    }
  }
  
  implicit object RMFormat6 extends TwoOperandFormat[RM, ModRM.reg, ModRM.reg] {

    def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedTwoOperands[ModRM.reg, ModRM.reg](operand1Size,operand2Size,opcode,prefix) {
      def getAddressingForm(op1: ModRM.reg, op2: ModRM.reg) = {
  
        InstructionFormat (
          OnlyModRM(ModRMReg(TwoRegisters, op1, op2)),
          immediate = None
        )
      }
  
      def size = prefix.size + opcode.size + 1
    }
  }
  
  implicit object MFormat5 extends OneOperandFormat[M, AbsoluteAddress[Constant32]] {

    def apply(operand1Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedOneOperand[AbsoluteAddress[Constant32]](operand1Size, opcode, prefix) {
      def getAddressingForm(operand: AbsoluteAddress[Constant32]) = {
            InstructionFormat (
              addressingForm = NoSIBWithDisplacement(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, new EBP), operand.displacement), //mem.encode(opcode.opcodeExtension),
              immediate = None
            )
      }
      
       def size = prefix.size + opcode.size + 1 + operand1Size
    }
  }
  
    implicit object AbsoluteAddress32 extends AbsoluteAddress[Constant32] {
      selff =>
        var offset = 0
      def displacement = Constant32(offset)
        
      def getRelative = new Relative32 {
        def displacement = Constant32(offset)
        def size = 4
      }
    }
  
  implicit object AbsoluteAddress64 extends AbsoluteAddress[Constant64] {
    selff =>
      var offset:Long = 0
      def displacement = Constant64(offset)
        
      def getRelative = new Relative64 {
        def displacement = Constant64(offset)
        def size = 4
      }
  }

  implicit object RMFormat2 extends TwoOperandFormat[RM, ModRM.reg, AbsoluteAddress[Constant32]] {

    def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedTwoOperands[ModRM.reg, AbsoluteAddress[Constant32]](operand1Size,operand2Size, opcode, prefix) {
      def getAddressingForm(op1: ModRM.reg, op2: AbsoluteAddress[Constant32]) = {
        InstructionFormat (
          addressingForm = NoSIBWithDisplacement(ModRMReg(NoDisplacement, op1, new EBP), op2.displacement),
          immediate = None
        )
      }
  
      def size = prefix.size + opcode.size + 1 + operand2Size
    }
  }
  
  implicit object RMFormat3 extends TwoOperandFormat[RM, ModRM.reg, RegisterIndirect[_32]] {

    def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedTwoOperands[ModRM.reg, RegisterIndirect[_32]](operand1Size,operand2Size, opcode, prefix) {
      def getAddressingForm(op1: ModRM.reg, op2: RegisterIndirect[_32]) = {
        InstructionFormat (
          addressingForm = OnlyModRM(ModRMReg(NoDisplacement, op1, rm = op2.base)),
          immediate = None
        )
      }
  
      def size = prefix.size + opcode.size + 1
    }
  }
  
  implicit object MRFormat2 extends TwoOperandFormat[MR, ModRM.reg, ModRM.reg] {

    def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedTwoOperands[ModRM.reg, ModRM.reg](operand1Size,operand2Size,opcode, prefix) {
      def getAddressingForm(op1: ModRM.reg, op2: ModRM.reg) = {
        RMFormat6(operand2Size, operand1Size, opcode, prefix).getAddressingForm(op2, op1)
      }
  
      def size = RMFormat6(operand2Size, operand1Size, opcode, prefix).size
    }
  }

  implicit object M1Format extends TwoOperandFormat[M1, Relative[DwordOperand], One] with Formats {
    def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedTwoOperands[Relative[DwordOperand], One](operand1Size, operand2Size, opcode, prefix) {
      def getAddressingForm(op1: Relative[DwordOperand],  op2: One) = MFormat(operand1Size, opcode, prefix).getAddressingForm(op1)
      def size = MFormat(operand1Size, opcode, prefix).size
    }
  }
  
  implicit object M1Format2 extends TwoOperandFormat[M1, RegisterIndirect[_32], One] with Formats {
    def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedTwoOperands[RegisterIndirect[_32], One](operand1Size, operand2Size, opcode, prefix) {
      def getAddressingForm(op1: RegisterIndirect[_32],  op2: One) = MFormat2(operand1Size, opcode, prefix).getAddressingForm(op1)
      def size = MFormat2(operand1Size, opcode, prefix).size
    }
  }
  
  implicit object M1Format3 extends TwoOperandFormat[M1, BaseIndex[r64,_], One] with Formats {
    def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedTwoOperands[BaseIndex[r64,_], One](operand1Size, operand2Size, opcode, prefix) {
      def getAddressingForm(op1: BaseIndex[r64,_],  op2: One) = MFormatB1(operand1Size, opcode, prefix).getAddressingForm(op1)
      def size = MFormatB1(operand1Size, opcode, prefix).size
    }
  }
  
  implicit object M1Format4 extends TwoOperandFormat[M1, GPR, One] with Formats {
    def apply(operand1Size: Int, operand2Size: Int, opcode: OpcodeFormat, prefix: Array[Byte]) = new ResolvedTwoOperands[GPR, One](operand1Size, operand2Size, opcode, prefix) {
      def getAddressingForm(op1: GPR,  op2: One) = MFormat4(operand1Size, opcode, prefix).getAddressingForm(op1)
      def size = MFormat4(operand1Size, opcode, prefix).size
    }
  }
}