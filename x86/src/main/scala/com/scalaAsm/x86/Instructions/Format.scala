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

trait LowPriorityFormats {

  implicit object MFormat32 extends OneOperandFormat[rel32, M] {

      def getAddressingForm(operand: rel32, opcode: OpcodeFormat) = {
            InstructionFormat (
              addressingForm = OnlyDisplacement(operand.displacement),
              immediate = None
            )
      }
      
       def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 4
  }
  
  implicit object MFormat64 extends OneOperandFormat[rel64, M] {

      def getAddressingForm(operand: rel64, opcode: OpcodeFormat) = {
            InstructionFormat (
              addressingForm = OnlyDisplacement(operand.displacement),
              immediate = None
            )
      }
      
       def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 8
  }
  
  trait MFormatIGeneric[X <: GeneralPurpose[_32]#Indirect] extends OneOperandFormat[X, M] {

      def getAddressingForm(operand: X, opcode: OpcodeFormat) = {
            InstructionFormat (
              addressingForm = OnlyModRM(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, operand.base)), //mem.encode(opcode.opcodeExtension),
              immediate = None
            )
      }
      
       def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1
  }
  
  implicit object MFormatIGeneric1 extends MFormatIGeneric[EAX#Indirect]
  implicit object MFormatIGeneric2 extends MFormatIGeneric[ECX#Indirect]
  implicit object MFormatIGeneric3 extends MFormatIGeneric[EDX#Indirect]
  implicit object MFormatIGeneric4 extends MFormatIGeneric[EBX#Indirect]
  implicit object MFormatIGeneric6 extends MFormatIGeneric[ESI#Indirect]
  implicit object MFormatIGeneric7 extends MFormatIGeneric[EDI#Indirect]
  // There is no [EBP], its slot is used by 32-disp only mode
  
  implicit object MFormat2R2 extends OneOperandFormat[GeneralPurpose[_64]#Indirect, M] {

      def getAddressingForm(operand: GeneralPurpose[_64]#Indirect, opcode: OpcodeFormat) = {
            InstructionFormat (
              addressingForm = OnlyModRM(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, operand.base)), //mem.encode(opcode.opcodeExtension),
              immediate = None
            )
      }
      
       def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1
  }
  
  implicit object MFormatB2 extends OneOperandFormat[r + _8, M] {

      def getAddressingForm(operand: r + _8, opcode: OpcodeFormat) = {
            InstructionFormat (
              NoSIBWithDisplacement(ModRMOpcode(DisplacementByte, opcode.opcodeExtension.get, operand.base), operand.displacement),
              immediate = None
            )
      }
      
       def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1 + 1
  }
  
  implicit object MFormat4 extends OneOperandFormat[ModRM.reg, M] {

      def getAddressingForm(operand: ModRM.reg, opcode: OpcodeFormat) = {
            InstructionFormat (
              addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcode.opcodeExtension.get, operand)),///reg.encode(opcode.opcodeExtension),
              immediate = None
            )
      }
      
       def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1
  }

  implicit object DSFormat extends OneOperandFormat[DS, DSFormat] {
      def getAddressingForm(op1: DS, opcode: OpcodeFormat) = NoAddressingForm
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size
  }

  implicit object CSFormat extends OneOperandFormat[CS, CSFormat] {
      def getAddressingForm(op1: CS, opcode: OpcodeFormat) = NoAddressingForm
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size
  }

  // abbreviated  reg/imm format, used with common instrctions like 'add(eax, byte(8))'
  implicit object I2Format extends TwoOperandFormat[ModRM.reg, imm, I2] {
      def getAddressingForm(op1: ModRM.reg, op2: imm, opcode: OpcodeFormat) = InstructionFormat (NoModRM(), Some(op2))
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1
  }

  implicit object IFormat8 extends OneOperandFormat[imm8, I] {

      def getAddressingForm(operand: imm8, opcode: OpcodeFormat) = {
        InstructionFormat (
          addressingForm = NoModRM(),
          immediate = Some(operand)
        )
      }
      
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1
  }
  
  implicit object IFormat16 extends OneOperandFormat[imm16, I] {

      def getAddressingForm(operand: imm16, opcode: OpcodeFormat) = {
        InstructionFormat (
          addressingForm = NoModRM(),
          immediate = Some(operand)
        )
      }
      
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 2
  }
  
  implicit object IFormat32 extends OneOperandFormat[imm32, I] {

      def getAddressingForm(operand: imm32, opcode: OpcodeFormat) = {
        InstructionFormat (
          addressingForm = NoModRM(),
          immediate = Some(operand)
        )
      }
      
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 4
  }
  
  implicit object IFormat64 extends OneOperandFormat[imm64, I] {

      def getAddressingForm(operand: imm64, opcode: OpcodeFormat) = {
        InstructionFormat (
          addressingForm = NoModRM(),
          immediate = Some(operand)
        )
      }
      
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 8
  }

  implicit object OffsetFormat extends OneOperandFormat[r64 + _, Offset] {

      def getAddressingForm(operand: r64 + _, opcode: OpcodeFormat) = {
        InstructionFormat (
          WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, operand.base), ScaleIndexByte(SIB.One, new ESP, operand.base)),
          immediate = None
        )
      }
      
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 2
  }
  
  implicit object OffsetFormat2 extends OneOperandFormat[r + _8, Offset] {

      def getAddressingForm(operand: r + _8, opcode: OpcodeFormat) = {
        InstructionFormat (
          NoSIBWithDisplacement(ModRMOpcode(DisplacementByte, opcode.opcodeExtension.get, operand.base), operand.displacement),
          immediate = None
        )
      }
      
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1 + 1
  }
  
  implicit object MRFormat extends TwoOperandFormat[r + _8, ModRM.reg, MR] {

      def getAddressingForm(op1: r + _8, op2: ModRM.reg, opcode: OpcodeFormat) = {
        RMFormat.getAddressingForm(op2, op1, opcode)
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = RMFormat.size(opcode, prefix)
  }
  
  implicit object RMFormat extends TwoOperandFormat[ModRM.reg, r + _8, RM] {
    
      def getAddressingForm(op1: ModRM.reg, op2: r + _8, opcode: OpcodeFormat) = {
        InstructionFormat (
            NoSIBWithDisplacement(ModRMReg(DisplacementByte, reg = op1, rm = op2.base), op2.displacement),
            immediate = None
        )
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1 + 1
  }
  
  implicit object MIFormat8 extends TwoOperandFormat[ModRM.reg, imm8, MI] {

      def getAddressingForm(op1: ModRM.reg, op2: imm8, opcode: OpcodeFormat) = {
          InstructionFormat (
            addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcode.opcodeExtension.get, op1)),
            immediate = Some(op2)
          )
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1 + 1
  }
  
  implicit object MIFormat16 extends TwoOperandFormat[ModRM.reg, imm16, MI] {

      def getAddressingForm(op1: ModRM.reg, op2: imm16, opcode: OpcodeFormat) = {
          InstructionFormat (
            addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcode.opcodeExtension.get, op1)),
            immediate = Some(op2)
          )
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1 + 2
  }
  
  implicit object MIFormat32 extends TwoOperandFormat[ModRM.reg, imm32, MI] {

      def getAddressingForm(op1: ModRM.reg, op2: imm32, opcode: OpcodeFormat) = {
          InstructionFormat (
            addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcode.opcodeExtension.get, op1)),
            immediate = Some(op2)
          )
      }
      
      override def getPrefix(prefix: Seq[Prefix]) = {
        if (prefix.exists(_.isInstanceOf[REX])) {
          REX(true, false, false, true).get
        } else {
          Array[Byte]()
        }
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1 + 4
  }
  
  implicit object MIFormat64 extends TwoOperandFormat[ModRM.reg, imm64, MI] {

      def getAddressingForm(op1: ModRM.reg, op2: imm64, opcode: OpcodeFormat) = {
          InstructionFormat (
            addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcode.opcodeExtension.get, op1)),
            immediate = Some(op2)
          )
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1 + 8
  }
  
  implicit object OIFormat64 extends TwoOperandFormat[ModRM.plusRd, imm64, OI] {

      def getAddressingForm(op1: ModRM.plusRd, op2: imm64, opcode: OpcodeFormat) = {
        InstructionFormat (
          addressingForm = NoModRM(),
          immediate = Some(op2)
        )
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 8
  }
  
  implicit object OIFormat32 extends TwoOperandFormat[ModRM.plusRd, imm32, OI] {

      def getAddressingForm(op1: ModRM.plusRd, op2: imm32, opcode: OpcodeFormat) = {
        InstructionFormat (
          addressingForm = NoModRM(),
          immediate = Some(op2)
        )
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 4
  }
  
  implicit object OIFormat16 extends TwoOperandFormat[ModRM.plusRd, imm16, OI] {

      def getAddressingForm(op1: ModRM.plusRd, op2: imm16, opcode: OpcodeFormat) = {
        InstructionFormat (
          addressingForm = NoModRM(),
          immediate = Some(op2)
        )
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 2
  }
  
  implicit object OIFormat8 extends TwoOperandFormat[ModRM.plusRd, imm8, OI] {

      def getAddressingForm(op1: ModRM.plusRd, op2: imm8, opcode: OpcodeFormat) = {
        InstructionFormat (
          addressingForm = NoModRM(),
          immediate = Some(op2)
        )
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1
  }
  
  implicit object RMFormat2 extends TwoOperandFormat[ModRM.reg, AbsoluteAddress[_32], RM] {

      def getAddressingForm(op1: ModRM.reg, op2: AbsoluteAddress[_32], opcode: OpcodeFormat) = {
        InstructionFormat (
          addressingForm = NoSIBWithDisplacement(ModRMReg(NoDisplacement, op1, new EBP), op2.displacement),
          immediate = None
        )
      }
      
      override def getPrefix(prefix: Seq[Prefix]) = {
        if (prefix.exists(_.isInstanceOf[REX])) {
          REX(true, false, false, false).get
        } else {
          Array[Byte]()
        }
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1 + 4
  }
}

trait Formats extends LowPriorityFormats {
  
  implicit object MFormat2R3 extends OneOperandFormat[RSP#Indirect, M] {

      def getAddressingForm(operand: RSP#Indirect, opcode: OpcodeFormat) = {
        InstructionFormat (
          WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, operand.base), ScaleIndexByte(SIB.One, new ESP, operand.base)),
          immediate = None
        )
      }
      
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 2
  }
  
  implicit object OFormat extends OneOperandFormat[ModRM.plusRd, O] {
      def getAddressingForm(operand: ModRM.plusRd, opcode: OpcodeFormat) = {
        InstructionFormat (
          addressingForm = NoModRM(),
          immediate = None
        )
      }
      
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size
  }
  
  implicit object MFormatB1 extends OneOperandFormat[r64 + _, M] {

      def getAddressingForm(operand: r64 + _, opcode: OpcodeFormat) = {
            InstructionFormat (
              WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, operand.base), ScaleIndexByte(SIB.One, new ESP, operand.base)),
              immediate = None
            )
      }
      
       def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 2
  }
  
  implicit object RMFormatB1 extends TwoOperandFormat[ModRM.reg, StackPointer[_] + _8, RM] {
      def getAddressingForm(op1: ModRM.reg, op2: StackPointer[_] + _8, opcode: OpcodeFormat) = {
        InstructionFormat (
      	    WithSIBWithDisplacement(ModRMReg(DisplacementByte, op1, op2.base), ScaleIndexByte(SIB.One, new ESP, op2.base), op2.displacement),
            immediate = None
        )
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 2 + 1
  }
  
  implicit object RMFormatB2 extends TwoOperandFormat[ModRM.reg, r + _32, RM] {

      def getAddressingForm(op1: ModRM.reg, op2: r + _32, opcode: OpcodeFormat) = {
        InstructionFormat (
      	    NoSIBWithDisplacement(ModRMReg(DisplacementDword, reg = op1, rm = op2.base), op2.displacement),  
            immediate = None
        )
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1 + 4
  }
  
  implicit object RMFormat6 extends TwoOperandFormat[ModRM.reg, ModRM.reg, RM] {

      def getAddressingForm(op1: ModRM.reg, op2: ModRM.reg, opcode: OpcodeFormat) = {
  
        InstructionFormat (
          OnlyModRM(ModRMReg(TwoRegisters, op1, op2)),
          immediate = None
        )
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1
  }
  
  implicit object MFormat5 extends OneOperandFormat[AbsoluteAddress[_32], M] {

      def getAddressingForm(operand: AbsoluteAddress[_32], opcode: OpcodeFormat) = {
            InstructionFormat (
              addressingForm = NoSIBWithDisplacement(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, new EBP), operand.displacement), //mem.encode(opcode.opcodeExtension),
              immediate = None
            )
      }
      
       def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1 + 4
  }
  
    implicit object AbsoluteAddress32 extends AbsoluteAddress[_32] {
      selff =>
        var offset = 0
      def displacement = Constant32(offset)
        
      def getRelative = new Relative32 {
        def displacement = Constant32(offset)
        def size = 4
      }
    }
  
  implicit object AbsoluteAddress64 extends AbsoluteAddress[_64] {
    selff =>
      var offset:Long = 0
      def displacement = Constant64(offset)
        
      def getRelative = new Relative64 {
        def displacement = Constant64(offset)
        def size = 4
      }
  }

  implicit object RMFormat264 extends TwoOperandFormat[Extra64Reg, AbsoluteAddress[_32], RM] {

      def getAddressingForm(op1: Extra64Reg, op2: AbsoluteAddress[_32], opcode: OpcodeFormat) = {
        InstructionFormat (
          addressingForm = NoSIBWithDisplacement(ModRMReg(NoDisplacement, op1, new EBP), op2.displacement),
          immediate = None
        )
      }
      
      override def getPrefix(prefix: Seq[Prefix]) = {
        if (prefix.exists(_.isInstanceOf[REX])) {
          REX(true, true, false, false).get
        } else {
          Array[Byte]()
        }
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1 + 4
  }
  
  implicit object RMFormat3 extends TwoOperandFormat[ModRM.reg, r32#Indirect, RM] {

      def getAddressingForm(op1: ModRM.reg, op2: r32#Indirect, opcode: OpcodeFormat) = {
        InstructionFormat (
          addressingForm = OnlyModRM(ModRMReg(NoDisplacement, op1, rm = op2.base)),
          immediate = None
        )
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = prefix.size + opcode.size + 1
  }
  
  implicit object MRFormat2 extends TwoOperandFormat[ModRM.reg, ModRM.reg, MR] {

      def getAddressingForm(op1: ModRM.reg, op2: ModRM.reg, opcode: OpcodeFormat) = {
        RMFormat6.getAddressingForm(op2, op1, opcode)
      }
  
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = RMFormat6.size(opcode, prefix)
  }

  implicit object M1Format extends TwoOperandFormat[rel32, One, M1] with Formats {
      def getAddressingForm(op1: rel32,  op2: One, opcode: OpcodeFormat) = MFormat32.getAddressingForm(op1, opcode)
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = MFormat32.size(opcode, prefix)
  }
  
  implicit object M1Format2 extends TwoOperandFormat[r32#Indirect, One, M1] with Formats {
      def getAddressingForm(op1: r32#Indirect,  op2: One, opcode: OpcodeFormat) = new MFormatIGeneric[r32#Indirect]{}.getAddressingForm(op1, opcode)
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = new MFormatIGeneric[r32#Indirect]{}.size(opcode, prefix)
  }
  
  implicit object M1Format3 extends TwoOperandFormat[r64 + _, One, M1] with Formats {
      def getAddressingForm(op1: r64+_,  op2: One, opcode: OpcodeFormat) = MFormatB1.getAddressingForm(op1, opcode)
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = MFormatB1.size(opcode, prefix)
  }
  
  implicit object M1Format4 extends TwoOperandFormat[ModRM.reg, One, M1] with Formats {
      def getAddressingForm(op1: ModRM.reg,  op2: One, opcode: OpcodeFormat) = MFormat4.getAddressingForm(op1, opcode)
      def size(opcode: OpcodeFormat, prefix: Seq[Prefix]) = MFormat4.size(opcode, prefix)
  }
}