package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

trait Instruction

trait x86Instruction extends Instruction {
  val mnemonic: String
  implicit def toByte(x: Int) = x.toByte
  implicit def toOneOpcode(x: Int): OneOpcode = OneOpcode(x.toByte)
  implicit def toTwoOpcodes(x: (Int, Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte)
}

trait ZeroOperandInstruction extends x86Instruction {
  def opcode: OpcodeFormat

  def apply: MachineCodeBuilder =
    new MachineCodeBuilder {
      def get = new MachineCode {
        val size = getSize
        val code = getBytes
        val line = mnemonic
      }
    }

  def getSize: Int = {
    opcode.size
  }

  def getBytes: Array[Byte] = {
    opcode.get(null)
  }
}

trait M
trait O
trait I
trait Offset

trait Formats {
  
  implicit object MFormat extends OneOperandFormat2[M, ModRM.rm] {

    def getAddressingForm(op1: ModRM.rm, opcode: OpcodeFormat) = {

      op1 match {
        case rel: Relative =>
          new AddressingFormSpecifierTemp {
            val addressingForm = NoModRM()
            val (displacment, immediate) = (rel.offset, None)
          }
        case mem: Memory =>
          new AddressingFormSpecifierTemp {
            val addressingForm = OneOperandInstructionFormatter.encode(mem, opcode.opcodeExtension)
            val (displacment, immediate) = (mem.offset, mem.immediate)
          }
        case reg: GPR =>
          new AddressingFormSpecifierTemp {
            val addressingForm = OneOperandInstructionFormatter.encode(reg, opcode.opcodeExtension)
            val (displacment, immediate) = (None, None)
          }
      }
    }
  }
  
  implicit object DSFormat extends OneOperandFormat2[DS, DS] {
    def getAddressingForm(op1: DS, opcode: OpcodeFormat) = NoAddressingForm
  }
  
  implicit object CSFormat extends OneOperandFormat2[CS, CS] {
    def getAddressingForm(op1: CS, opcode: OpcodeFormat) = NoAddressingForm
  }
  
  implicit object OFormat extends OneOperandFormat2[O, ModRM.plusRd] {
    def getAddressingForm(op1: ModRM.plusRd, opcode: OpcodeFormat) = {
      new AddressingFormSpecifierTemp {
        val addressingForm = NoModRM()
        val (displacment, immediate) = (None, None)
      }
    }
  }

  implicit object IFormat extends OneOperandFormat2[I, Immediate] {

    def getAddressingForm(op1: Immediate, opcode: OpcodeFormat) = {
      new AddressingFormSpecifierTemp {
        val addressingForm = NoModRM()
        val (displacment, immediate) = (None, Some(op1))
      }
    }

  }

  implicit object OffsetFormat extends OneOperandFormat2[Offset, Memory] {

    def getAddressingForm(op1: Memory, opcode: OpcodeFormat) = {
      new AddressingFormSpecifierTemp {
        val addressingForm = NoSIB(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, new EBP))
        val (displacment, immediate) = (op1.offset, None)
      }
    }
  }
}

abstract class OneOperandInstruction[OpEn, -O1 <: Operand](implicit format: OneOperandFormat2[OpEn, O1]) extends x86Instruction with Formats {
  val opcode: OpcodeFormat

  def apply(x: O1) =
    new MachineCodeBuilder1[O1](x) {
      def get = new MachineCode {
        val size = getSize(x)
        val code = getBytes(x)
        val line = mnemonic
      }
    }

  def getSize(x: O1): Int = {
    opcode.size + format.getAddressingForm(x, opcode).size
  }

  def getBytes(x: O1): Array[Byte] = {
    opcode.get(x) ++ format.getAddressingForm(x, opcode).getBytes
  }
  
  //def form[X <: O1](x:X)(implicit format: OneOperandFormat[O1]) = format.getAddressingForm(x, opcode)
}

trait TwoOperandInstruction[-O1 <: Operand, -O2 <: Operand] extends x86Instruction {
  val opcode: OpcodeFormat
  def opEn: TwoOperandsFormat[O1, O2]

  def apply(x: O1, y: O2) =
    new MachineCodeBuilder2[O1, O2](x, y) {
      def get = new MachineCode {
        val size = getSize(x, y)
        val code = getBytes(x, y)
        val line = mnemonic + " " + opEn.toString
      }
    }

  def getSize(x: O1, y: O2): Int = {
    val prefixes = opEn.getPrefixes(x, y) getOrElse Array()
    prefixes.size + opcode.size + opEn.getAddressingForm(x, y, opcode).size
  }

  def getBytes(x: O1, y: O2): Array[Byte] = {
    val prefixes = opEn.getPrefixes(x, y) getOrElse Array()
    prefixes ++ opcode.get(x) ++ opEn.getAddressingForm(x, y, opcode).getBytes
  }
}
