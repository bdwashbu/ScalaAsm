package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory.ModRM
import com.scalaAsm.x86.Operands.Memory.InstructionFormat
import com.scalaAsm.x86.Operands.Memory.NoModRM
import com.scalaAsm.x86.Operands.Memory.NoSIBWithDisplacement
import com.scalaAsm.x86.Operands.Memory.NoDisplacement
import com.scalaAsm.x86.Operands.Memory.ModRMOpcode
import com.scalaAsm.x86.Operands.Memory.OnlyDisplacement
import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.x86.Operands.Memory.RegisterIndirect
import com.scalaAsm.x86.Operands.Memory.BaseIndex
import com.scalaAsm.x86.Operands.One
import com.scalaAsm.x86.Operands.Memory.OnlyDisplacement
import com.scalaAsm.x86.Operands.Memory.NoSIBWithDisplacement
import com.scalaAsm.x86.Operands.Memory.OnlyModRM
import com.scalaAsm.x86.Operands.Memory.TwoRegisters
import com.scalaAsm.x86.Operands.Memory.ModRMReg
import com.scalaAsm.x86.Operands.Memory.Relative32
import com.scalaAsm.x86.Operands.Memory.Relative64

trait Instruction
trait SizedInstructionField {
  def size: Int
}

trait InstructionField extends SizedInstructionField {
  def getBytes: Array[Byte]
}

trait x86Instruction extends Instruction {
  import scala.language.implicitConversions
  val opcode: OpcodeFormat
  val mnemonic: String
  implicit def toByte(x: Int) = x.toByte
  implicit def toOneOpcode(x: Int): OneOpcode = OneOpcode(x.toByte)
  implicit def toTwoOpcodes(x: (Int, Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte)
}

trait NP

trait M
trait O
trait I
trait Offset

trait OperandEncoding {
  type Immediate = Constant
}

trait LowPriorityFormats extends OperandEncoding {

  implicit object MFormat extends OneOperandFormat[M, Relative {type Size = DwordOperand}] {

    def getAddressingForm(operand: Relative{type Size = DwordOperand}, opcode: OpcodeFormat): InstructionFormat = {

          InstructionFormat (
            addressingForm = OnlyDisplacement(operand.displacement),
            immediate = None
          )
        
    }
    
     def getPrefixes(operand: Relative{type Size = DwordOperand}): Option[Array[Byte]] = None
  }
  
  implicit object MFormat2 extends OneOperandFormat[M, RegisterIndirect] {

    def getAddressingForm(operand: RegisterIndirect, opcode: OpcodeFormat): InstructionFormat = {
          InstructionFormat (
            addressingForm = OnlyModRM(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, operand.base)), //mem.encode(opcode.opcodeExtension),
            immediate = None
          )
    }
    
     def getPrefixes(operand: RegisterIndirect): Option[Array[Byte]] = None
  }
  
  implicit object MFormat3 extends OneOperandFormat[M, BaseIndex] {

    def getAddressingForm(operand: BaseIndex, opcode: OpcodeFormat): InstructionFormat = {
          InstructionFormat (
            addressingForm = operand.encode(opcode.opcodeExtension),
            immediate = None
          )
    }
    
     def getPrefixes(operand: BaseIndex): Option[Array[Byte]] = None
  }
  
  implicit object MFormat4 extends OneOperandFormat[M, GPR] {

    def getAddressingForm(operand: GPR, opcode: OpcodeFormat): InstructionFormat = {
          InstructionFormat (
            addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcode.opcodeExtension.get, operand)),///reg.encode(opcode.opcodeExtension),
            immediate = None
          )
    }
    
     def getPrefixes(operand: GPR): Option[Array[Byte]] = None
  }

  implicit object DSFormat extends OneOperandFormat[DS, DS] {
    def getAddressingForm(op1: DS, opcode: OpcodeFormat) = NoAddressingForm
    def getPrefixes(op1: DS) = None
  }

  implicit object CSFormat extends OneOperandFormat[CS, CS] {
    def getAddressingForm(op1: CS, opcode: OpcodeFormat) = NoAddressingForm
    def getPrefixes(op1: CS) = None
  }

  implicit object OFormat extends OneOperandFormat[O, ModRM.plusRd] {
    def getAddressingForm(operand: ModRM.plusRd, opcode: OpcodeFormat) = {
      InstructionFormat (
        addressingForm = NoModRM(),
        immediate = None
      )
    }
    
    def getPrefixes(operand: ModRM.plusRd): Option[Array[Byte]] = None
  }

  implicit object IFormat extends OneOperandFormat[I, Immediate] {

    def getAddressingForm(operand: Immediate, opcode: OpcodeFormat) = {
      InstructionFormat (
        addressingForm = NoModRM(),
        immediate = Some(operand)
      )
    }
    
    def getPrefixes(operand: Immediate): Option[Array[Byte]] = None
  }

  implicit object OffsetFormat extends OneOperandFormat[Offset, BaseIndex] {

    def getAddressingForm(operand: BaseIndex, opcode: OpcodeFormat) = {
      InstructionFormat (
        addressingForm = operand.encode(opcode.opcodeExtension),
        immediate = None
      )
    }
    
    def getPrefixes(operand: BaseIndex): Option[Array[Byte]] = None
  }
}

trait MR
trait OI
trait RM
trait M1
trait MI

trait Formats extends LowPriorityFormats {

  implicit object MFormat5 extends OneOperandFormat[M, AbsoluteAddress[Constant32]] {

    def getAddressingForm(operand: AbsoluteAddress[Constant32], opcode: OpcodeFormat): InstructionFormat = {

          InstructionFormat (
            addressingForm = NoSIBWithDisplacement(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, new EBP), operand.displacement), //mem.encode(opcode.opcodeExtension),
            immediate = None
          )
        
    }
    
     def getPrefixes(operand: AbsoluteAddress[Constant32]): Option[Array[Byte]] = None
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
  
  implicit object MIFormat extends TwoOperandFormat[MI, ModRM.rm, Immediate] {

    def getAddressingForm(op1: ModRM.rm, op2: Immediate, opcode: OpcodeFormat): InstructionFormat = {

      op1 match {
        case reg: GPR =>
          InstructionFormat (
            addressingForm = OnlyModRM(ModRMOpcode(TwoRegisters, opcode.opcodeExtension.get, reg)),//reg.encode(opcode.opcodeExtension),
            immediate = Some(op2)
          )
      }
    }

    def getPrefixes(op1: ModRM.rm, op2: Immediate): Option[Array[Byte]] = {
      op1 match {
        case reg: UniformByteRegister =>
          Some(REX.W(false).get)
        case reg: Register64 =>
          Some(REX.W(true).get)
        case _ => None
      }
    }
  }

  implicit object RMFormat extends TwoOperandFormat[RM, ModRM.reg, ModRM.rm] {

    def getAddressingForm(op1: ModRM.reg, op2: ModRM.rm, opcode: OpcodeFormat): InstructionFormat = {

      InstructionFormat (
        addressingForm = op2 match {
        case mem: AbsoluteAddress[_] =>
          NoSIBWithDisplacement(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, new EBP), mem.displacement)
        case mem: RegisterIndirect =>
          OnlyModRM(ModRMReg(NoDisplacement, op1, rm = mem.base))//mem.encode(operands._1, opcode.opcodeExtension)
        case mem: BaseIndex =>
          mem.encode(op1, opcode.opcodeExtension)
        case reg: GPR =>
          op1.encode(reg, opcode.opcodeExtension)
      },
        immediate = None
      )
      
      
    }

    def getPrefixes(op1: ModRM.reg, op2: ModRM.rm): Option[Array[Byte]] = {
      op1 match {
        case reg: UniformByteRegister =>
          Some(REX.W(false).get)
        case reg: Register64 =>
          Some(REX.W(true).get)
        case _ => None
      }
    }
  }

  implicit object MRFormat extends TwoOperandFormat[MR, ModRM.rm, ModRM.reg] {

    def getAddressingForm(op1: ModRM.rm, op2: ModRM.reg, opcode: OpcodeFormat): InstructionFormat = {
      RMFormat.getAddressingForm(op2, op1, opcode)
    }

    def getPrefixes(op1: ModRM.rm, op2: ModRM.reg): Option[Array[Byte]] = RMFormat.getPrefixes(op2, op1)
  }

  implicit object OIFormat extends TwoOperandFormat[OI, ModRM.plusRd, Immediate] {

    def getAddressingForm(op1: ModRM.plusRd, op2: Immediate, opcode: OpcodeFormat): InstructionFormat = {
      InstructionFormat (
        addressingForm = NoModRM(),
        immediate = Some(op2)
      )
    }

    def getPrefixes(op1: ModRM.plusRd, op2: Immediate): Option[Array[Byte]] = {
      op1 match {
        case reg: Register64 =>
          Some(REX.W(true).get)
        case _ => None
      }
    }
  }

  implicit object M1Format extends TwoOperandFormat[M1, Relative{type Size = DwordOperand}, One] with Formats {
    def getAddressingForm(op1: Relative{type Size = DwordOperand},  op2: One, opcode: OpcodeFormat): InstructionFormat = MFormat.getAddressingForm(op1, opcode)
    def getPrefixes(op1: Relative{type Size = DwordOperand}, op2: One): Option[Array[Byte]] = MFormat.getPrefixes(op1)
  }
  
  implicit object M1Format2 extends TwoOperandFormat[M1, RegisterIndirect, One] with Formats {
    def getAddressingForm(op1: RegisterIndirect,  op2: One, opcode: OpcodeFormat): InstructionFormat = MFormat2.getAddressingForm(op1, opcode)
    def getPrefixes(op1: RegisterIndirect, op2: One): Option[Array[Byte]] = MFormat2.getPrefixes(op1)
  }
  
  implicit object M1Format3 extends TwoOperandFormat[M1, BaseIndex, One] with Formats {
    def getAddressingForm(op1: BaseIndex,  op2: One, opcode: OpcodeFormat): InstructionFormat = MFormat3.getAddressingForm(op1, opcode)
    def getPrefixes(op1: BaseIndex, op2: One): Option[Array[Byte]] = MFormat3.getPrefixes(op1)
  }
  
  implicit object M1Format4 extends TwoOperandFormat[M1, GPR, One] with Formats {
    def getAddressingForm(op1: GPR,  op2: One, opcode: OpcodeFormat): InstructionFormat = MFormat4.getAddressingForm(op1, opcode)
    def getPrefixes(op1: GPR, op2: One): Option[Array[Byte]] = MFormat4.getPrefixes(op1)
  }
}

trait MachineCodeBuilder {
  def get: MachineCode
}

class OneMachineCodeBuilder[O1 <: Operand, X](operand: O1, opcode: OpcodeFormat, mnemonic: String, format: OneOperandFormat[X, O1]) extends MachineCodeBuilder {
  def get() =
    new MachineCode {
        val size = getSize
        val code = getBytes
        val line = mnemonic
      }

  def getSize: Int = {
    val prefixes = format.getPrefixes(operand) getOrElse Array()
    prefixes.size + opcode.size + format.getAddressingForm(operand, opcode).size
  }

  private def getBytes: Array[Byte] = {
    val prefixes = format.getPrefixes(operand) getOrElse Array()
    prefixes ++: opcode.get(OneOperand(operand)) ++: format.getAddressingForm(operand, opcode).getBytes
  }
}

class TwoMachineCodeBuilder[O1 <: Operand, O2 <: Operand, X](operand: O1, operand2: O2, opcode: OpcodeFormat, mnemonic: String, format: TwoOperandFormat[X, O1, O2]) extends MachineCodeBuilder {
  def get() =
    new MachineCode {
        val size = getSize
        val code = getBytes
        val line = mnemonic
      }

  def getSize: Int = {
    val prefixes = format.getPrefixes(operand, operand2) getOrElse Array()
    prefixes.size + opcode.size + format.getAddressingForm(operand, operand2, opcode).size
  }

  private def getBytes: Array[Byte] = {
    val prefixes = format.getPrefixes(operand, operand2) getOrElse Array()
    prefixes ++: opcode.get(TwoOperands(operand, operand2)) ++: format.getAddressingForm(operand, operand2, opcode).getBytes
  }
}

abstract class ZeroOperandInstruction extends x86Instruction with Formats {
  def get[X] = new OneMachineCodeBuilder(Constant8(0), opcode, mnemonic, new NoOperandFormat[X, Constant8] {}) {}
}

abstract class OneOperandInstruction[OpEn, -O1 <: Operand] extends x86Instruction with Formats {
  def get[X <: O1](x: X, format: OneOperandFormat[OpEn, X]) = new OneMachineCodeBuilder(x, opcode, mnemonic, format) {}
}

abstract class TwoOperandInstruction[OpEn, -O1 <: Operand, -O2 <: Operand] extends x86Instruction with Formats {
  def get[X <: O1, Y <: O2](x: X, y:Y, format: TwoOperandFormat[OpEn, X, Y]) = new TwoMachineCodeBuilder(x, y, opcode, mnemonic, format) {}
}

abstract class TwoOperandSizedInstruction[OpEn, -O1 <: Operand, -O2 <: Operand] extends x86Instruction with Formats {
  def get[X <: O1, Y <: O2](x: X, y:Y, size: InstructionSize[X,Y], format: TwoOperandFormat[OpEn, X,Y]) = new TwoMachineCodeBuilder(x, y, opcode, mnemonic, format) {
    override def getSize: Int = size.getSize
  }
}
