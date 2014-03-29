package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

trait OperandFormat


abstract class NoOperand extends OperandFormat {
  //def getAddressingForm(opcode: OpcodeFormat): Option[AddressingFormSpecifier]
}

abstract class OneOperandFormat[-X] extends OperandFormat {
 // override def toString = x.toString
  def getAddressingForm(x:X, opcode: OpcodeFormat): Option[AddressingFormSpecifier]
}

abstract class TwoOperandsFormat[-X, -Y] extends OperandFormat {
  //override def toString = x.toString + ", " + y.toString
  def getAddressingForm(x:X,y:Y,opcode: OpcodeFormat): Option[AddressingFormSpecifier]
}

trait InstructionFormat {

  trait DualOperandEncoding[-O1 <: ModRM.rm, -O2 <: Operand] extends DualOperand[O1, O2] {
    def encode(opcodeExtend: Option[Byte]): (Option[ModRM], Option[SIB])
  }

  def getEncoding[O1 <: ModRM.rm, O2 <: Operand](op1: O1, op2: O2, opcodeExtend: Option[Byte])(implicit operands: DualOperandEncoding[O1, O2]) = {
    operands.set(op1, op2)
    operands.encode(opcodeExtend)
  }

  trait SingleOperandEncoding[-O1 <: Operand] extends SingleOperand[O1] {
    def encode(opcodeExtend: Option[Byte]): (Option[ModRM], Option[SIB])
  }

  def getEncoding[O1 <: ModRM.rm](op1: O1, opcodeExtend: Option[Byte])(implicit operand: SingleOperandEncoding[O1]) = {
    operand.set(op1)
    operand.encode(opcodeExtend)
  }

  implicit object test1 extends DualOperandEncoding[GPR, Immediate] {
    def encode(opcodeExtend: Option[Byte]): (Option[ModRM], Option[SIB]) = {
      val modRM: Option[ModRM] = Some(ModRMOpcode(TwoRegisters, opcodeExtend.get, op1))
      (modRM, None)
    }
  }

  implicit object test2 extends DualOperandEncoding[GPR, Memory] {
    def encode(opcodeExtend: Option[Byte]): (Option[ModRM], Option[SIB]) = {
      (op2.base, op2.offset, op2.immediate) match {
        case (Some(base), offset @ Some(off: Displacement8), None) if base.ID == 4 =>
          val modRM = Some(ModRMReg(DisplacementByte, op1, base))
          val sib = Some(SIB(One, new ESP, base))
          (modRM, sib)
        case (Some(base), offset @ Some(off: Displacement32), None) =>
          val modRM = Some(ModRMReg(DisplacementDword, reg = op1, rm = base))
          (modRM, None)
        case (Some(base), offset @ Some(_: Displacement), None) =>
          val modRM = Some(ModRMReg(DisplacementByte, reg = op1, rm = base))
          (modRM, None)
        case (Some(base), None, None) =>
          val modRM = Some(ModRMReg(NoDisplacement, reg = op1, rm = base))
          (modRM, None)
      }
    }
  }

  implicit object test3 extends DualOperandEncoding[GPR, GPR] {
    def encode(opcodeExtend: Option[Byte]): (Option[ModRM], Option[SIB]) = {
      val modRM: Option[ModRM] = Some(ModRMReg(TwoRegisters, op1, op2))
      (modRM, None)
    }
  }

  implicit object test4 extends SingleOperandEncoding[Memory] {
    def encode(opcodeExtend: Option[Byte]): (Option[ModRM], Option[SIB]) = {
      (op1.base, op1.offset, op1.immediate) match {
        case (Some(base), offset @ Some(_: Displacement8), None) =>
          val modRM = Some(ModRMOpcode(DisplacementByte, opcodeExtend.get, base))
          (modRM, None)
        case (Some(base), None, None) =>
          val modRM = Some(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
          (modRM, None)
        case (None, None, imm @ Some(_: Immediate)) =>
          val modRM = Some(ModRMOpcode(NoDisplacement, opcodeExtend.get, new EBP))
          (modRM, None)
        case _ => (None, None)
      }
    }
  }
  
  implicit object test5 extends SingleOperandEncoding[GPR] {
    def encode(opcodeExtend: Option[Byte]): (Option[ModRM], Option[SIB]) = {
          val modRM = Some(ModRMOpcode(TwoRegisters, opcodeExtend.get, op1))
          (modRM, None)
    }
  }

  case class MI[M <: ModRM.rm, I <: Immediate] extends TwoOperandsFormat[M, I] {

    def getAddressingForm(op1: M, op2: I, opcode: OpcodeFormat): Option[AddressingFormSpecifier] = {

      op1 match {
        case reg: GPR =>
          Some(new AddressingFormSpecifier {
            val (modRM, sib) = getEncoding(reg, op2, opcode.opcodeExtension)
            val (displacment, immediate) = (None, Some(op2))
          })
      }
    }
  }

  case class RM[R <: ModRM.reg, M <: ModRM.rm] extends TwoOperandsFormat[R, M] {

    def getAddressingForm(op1: R, op2: M, opcode: OpcodeFormat): Option[AddressingFormSpecifier] = {

      op2 match {
        case mem: Memory =>
          Some(new AddressingFormSpecifier {
            val (modRM, sib) = getEncoding(op1, mem, opcode.opcodeExtension)
            val (displacment, immediate) = (mem.offset, mem.immediate)
          })
        case reg: GPR =>
          Some(new AddressingFormSpecifier {
            val (modRM, sib) = getEncoding(op1, reg, opcode.opcodeExtension)
            val (displacment, immediate) = (None, None)
          })
      }
    }

  }

  case class MR[M <: ModRM.rm, R <: ModRM.reg] extends TwoOperandsFormat[M, R] {

    def getAddressingForm(op1: M, op2: R, opcode: OpcodeFormat): Option[AddressingFormSpecifier] = {
      RM().getAddressingForm(op2, op1, opcode)
    }

  }

  case class O[O <: ModRM.plusRd] extends OneOperandFormat[O] {
    def getAddressingForm(op1: O, opcode: OpcodeFormat): Option[AddressingFormSpecifier] = {
      Some(new AddressingFormSpecifier {
        val modRM = None
        val (sib, displacment, immediate) = (None, None, None)
      })
    }
  }

  case class OI[O <: ModRM.plusRd, I <: Immediate] extends TwoOperandsFormat[O, I] {

    def getAddressingForm(op1: O, op2: I, opcode: OpcodeFormat): Option[AddressingFormSpecifier] = {
      Some(new AddressingFormSpecifier {
        val modRM = None
        val (sib, displacment, immediate) = (None, None, Some(op2))
      })
    }

  }

  case class I[I <: Immediate] extends OneOperandFormat[I] {

    def getAddressingForm(op1: I, opcode: OpcodeFormat): Option[AddressingFormSpecifier] = {
      Some(new AddressingFormSpecifier {
        val modRM = None
        val (sib, displacment, immediate) = (None, None, Some(op1))
      })
    }

  }

  case class Offset[O <: Memory] extends OneOperandFormat[O] {

    def getAddressingForm(op1: O, opcode: OpcodeFormat): Option[AddressingFormSpecifier] = {
      Some(new AddressingFormSpecifier {
        val modRM = Some(ModRMOpcode(NoDisplacement, opcode.opcodeExtension.get, new EBP))
        val (sib, displacment, immediate) = (None, op1.offset, None)
      })
    }
  }

  case class M[M <: ModRM.rm] extends OneOperandFormat[M] {

    def getAddressingForm(op1: M, opcode: OpcodeFormat): Option[AddressingFormSpecifier] = {

      op1 match {
        case rel: Relative =>
          Some(new AddressingFormSpecifier {
            val modRM = None
            val (sib, displacment, immediate) = (None, rel.offset, None)
          })
        case mem: Memory =>
          Some(new AddressingFormSpecifier {
            val (modRM, sib) = getEncoding(mem, opcode.opcodeExtension)
            val (displacment, immediate) = (mem.offset, mem.immediate)
          })

        case reg: GPR =>
          if (!opcode.opcodeExtension.isDefined) None else
            Some(new AddressingFormSpecifier {
              val (modRM, sib) = getEncoding(reg, opcode.opcodeExtension)
              val (displacment, immediate) = (None, None)
            })
      }
    }
  }

  case class M1[M <: ModRM.rm] extends TwoOperandsFormat[M, One] {
    def getAddressingForm(op1: M, op2: One, opcode: OpcodeFormat): Option[AddressingFormSpecifier] = M().getAddressingForm(op1, opcode)
  }
}