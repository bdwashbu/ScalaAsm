package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

trait OperandFormat {
  def getAddressingForm: Option[AddressingFormSpecifier]
}

trait InstructionFormat {
  self: Instruction =>

  class NoOperand extends OperandFormat {
    def getAddressingForm = None
  }

  abstract class OneOperandFormat[X <: Operand](x: X) extends OperandFormat {
    override def toString = x.toString
  }

  abstract class TwoOperandsFormat[X <: Operand, Y <: Operand](x: X, y: Y) extends OperandFormat {
    override def toString = x.toString + ", " + y.toString
  }

  trait DualOperandEncoding[-O1 <: ModRM.rm, -O2 <: Operand] extends DualOperand[O1,O2]{
    def encode: (Option[ModRM], Option[SIB])
  }

  def getEncoding[O1 <: ModRM.rm, O2 <: Operand](op1: O1, op2: O2)(implicit operands: DualOperandEncoding[O1, O2]) = {
    operands.set(op1, op2)
    operands.encode
  }
  
  trait SingleOperandEncoding[-O1 <: Operand] extends SingleOperand[O1]{
    def encode: (Option[ModRM], Option[SIB])
  }

  def getEncoding[O1 <: ModRM.rm](op1: O1)(implicit operand: SingleOperandEncoding[O1]) = {
    operand.set(op1)
    operand.encode
  }

  implicit object test1 extends DualOperandEncoding[GPR, Immediate] {
    def encode: (Option[ModRM], Option[SIB]) = {
      val modRM = Some(ModRM(TwoRegisters, opEx = opcode.opcodeExtension, rm = op1))
      (modRM, None)
    }
  }

  implicit object test2 extends DualOperandEncoding[GPR, Memory] {
    def encode: (Option[ModRM], Option[SIB]) = {
      (op2.base, op2.offset, op2.immediate) match {
        case (Some(base), offset @ Some(off: Displacement8), None) if base.ID == 4 =>
          val modRM = Some(ModRM(DisplacementByte, reg = Some(op1), rm = base))
          val sib = Some(SIB(One, new ESP, base))
          (modRM, sib)
        case (Some(base), offset @ Some(off: Displacement32), None) =>
          val modRM = Some(ModRM(DisplacementDword, reg = Some(op1), rm = base))
          (modRM, None)
        case (Some(base), offset @ Some(_: Displacement), None) =>
          val modRM = Some(ModRM(DisplacementByte, reg = Some(op1), rm = base))
          (modRM, None)
        case (Some(base), None, None) =>
          val modRM = Some(ModRM(NoDisplacement, reg = Some(op1), rm = base))
          (modRM, None)
      }
    }
  }

  implicit object test3 extends DualOperandEncoding[GPR, GPR] {
    def encode: (Option[ModRM], Option[SIB]) = {
      val modRM = Some(ModRM(TwoRegisters, reg = Some(op1), rm = op2))
      (modRM, None)
    }
  }
  
  implicit object test4 extends SingleOperandEncoding[Memory] {
    def encode: (Option[ModRM], Option[SIB]) = {
          (op1.base, op1.offset, op1.immediate, opcode.opcodeExtension.isDefined) match {
            case (Some(base), offset @ Some(_: Displacement8), None, true) =>
                val modRM = Some(ModRM(DisplacementByte, opEx = opcode.opcodeExtension, rm = base))
                (modRM, None)
            case (Some(base), None, None, true) =>
                val modRM = Some(ModRM(NoDisplacement, opEx = opcode.opcodeExtension, rm = base))
                (modRM, None)
            case (None, None, imm @ Some(_: Immediate), true) =>
                val modRM = Some(ModRM(NoDisplacement, opEx = opcode.opcodeExtension, rm = new EBP))
                (modRM, None)
            case _ => (None,None)
          }
    }
  }

  object NA extends NoOperand

  case class MI[M <: ModRM.rm, I <: Immediate](op1: M, op2: I) extends TwoOperandsFormat[M, I](op1, op2) {

    def getAddressingForm: Option[AddressingFormSpecifier] = {

      op1 match {
        case reg: GPR =>
          Some(new AddressingFormSpecifier {
            val (modRM, sib) = getEncoding(reg, op2)
            val (displacment, immediate) = (None, Some(op2))
          })
      }
    }
  }

  case class RM[R <: ModRM.reg, M <: ModRM.rm](op1: R, op2: M) extends TwoOperandsFormat[R, M](op1, op2) {

    def getAddressingForm: Option[AddressingFormSpecifier] = {

      op2 match {
        case mem: Memory =>
          Some(new AddressingFormSpecifier {
            val (modRM, sib) = getEncoding(op1, mem)
            val (displacment, immediate) = (mem.offset, mem.immediate)
          })
        case reg: GPR =>
          Some(new AddressingFormSpecifier {
            val (modRM, sib) = getEncoding(op1, reg)
            val (displacment, immediate) = (None, None)
          })
      }
    }

  }

  case class MR[M <: ModRM.rm, R <: ModRM.reg](op1: M, op2: R) extends TwoOperandsFormat[M, R](op1, op2) {

    def getAddressingForm: Option[AddressingFormSpecifier] = {
      RM(op2, op1).getAddressingForm
    }

  }

  case class O[R <: ModRM.reg](op1: R) extends OneOperandFormat[R](op1) {
    def getAddressingForm: Option[AddressingFormSpecifier] = new M(op1).getAddressingForm
  }

  case class OI[R <: ModRM.reg, I <: Immediate](op1: R, op2: I) extends TwoOperandsFormat[R, I](op1, op2) {

    def getAddressingForm: Option[AddressingFormSpecifier] = {
      Some(new AddressingFormSpecifier {
        val modRM = None
        val (sib, displacment, immediate) = (None, None, Some(op2))
      })
    }

  }

  case class I[I <: Immediate](op1: I) extends OneOperandFormat[I](op1) {

    def getAddressingForm: Option[AddressingFormSpecifier] = {
      Some(new AddressingFormSpecifier {
        val modRM = None
        val (sib, displacment, immediate) = (None, None, Some(op1))
      })
    }

  }

  case class Offset[O <: Memory](op1: O) extends OneOperandFormat[O](op1) {

    def getAddressingForm: Option[AddressingFormSpecifier] = {
      Some(new AddressingFormSpecifier {
        val modRM = Some(ModRM(NoDisplacement, opEx = opcode.opcodeExtension, rm = new EBP))
        val (sib, displacment, immediate) = (None, op1.offset, None)
      })
    }
  }

  case class M[M <: ModRM.rm](op1: M) extends OneOperandFormat[M](op1) {

    def getAddressingForm: Option[AddressingFormSpecifier] = {

      op1 match {
        case rel: Relative =>
          Some(new AddressingFormSpecifier {
            val modRM = None
            val (sib, displacment, immediate) = (None, rel.offset, None)
          })
        case mem: Memory =>
              Some(new AddressingFormSpecifier {
                val (modRM, sib) = getEncoding(mem)
                val (displacment, immediate) = (mem.offset, mem.immediate)
              })
          
        case reg: GPR =>
          if (!opcode.opcodeExtension.isDefined) None else
            Some(new AddressingFormSpecifier {
              val modRM = Some(ModRM(TwoRegisters, opEx = opcode.opcodeExtension, rm = reg))
              val (sib, displacment, immediate) = (None, None, None)
            })
      }
    }
  }

  case class M1[M <: ModRM.rm](op1: M) extends OneOperandFormat[M](op1) {
    def getAddressingForm: Option[AddressingFormSpecifier] = M(op1).getAddressingForm
  }
}