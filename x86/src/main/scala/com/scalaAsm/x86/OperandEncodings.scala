package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

trait OperandFormat {
  def getAddressingForm: Option[AddressingFormSpecifier]
}

trait OperandEncoding {
  self: Instruction =>

  class NoOperand extends OperandFormat {
    def getAddressingForm = None
  }

  abstract class OneOperandFormat[X](x: X) extends OperandFormat {
    override def toString = x.toString
  }

  abstract class TwoOperandsFormat[X, Y](x: X, y: Y) extends OperandFormat {
    override def toString = x.toString + ", " + y.toString
  }

  object NA extends NoOperand

  case class MI[M <: ModRM.rm, I <: Immediate](op1: M, op2: I) extends TwoOperandsFormat[M, I](op1, op2) {

    def getAddressingForm: Option[AddressingFormSpecifier] = {

      op1 match {
        case reg: GPR =>
	      Some(new AddressingFormSpecifier {
	        val modRM = Some(ModRM(TwoRegisters, opEx = opcode.opcodeExtension, rm = reg))
	        val sib = None
	        val displacment = None
	        val immediate = if (op2.value == 0) None else Some(op2)
	      })
      }
    }
  }

  case class RM[R <: ModRM.reg, M <: ModRM.rm](op1: R, op2: M) extends TwoOperandsFormat[R, M](op1, op2) {

    def getAddressingForm: Option[AddressingFormSpecifier] = {

      op2 match {
        case mem: Memory =>
          (mem.base, mem.offset, mem.immediate) match {
            case (Some(base), offset @ Some(off: Displacement8), None) if base.ID == 4 =>
              Some(new AddressingFormSpecifier {
                val modRM = Some(ModRM(DisplacementByte, reg = Some(op1), rm = base))
                val sib = Some(SIB(One, new NoneSib, base))
                val displacment = offset
                val immediate = None
              })
            case (Some(base), offset @ Some(off: Displacement32), None) =>
              Some(new AddressingFormSpecifier {
                val modRM = Some(ModRM(DisplacementDword, reg = Some(op1), rm = base))
                val (sib, displacment, immediate) = (None, offset, None)
              })
            case (Some(base), offset @ Some(_: Displacement), None) =>
              Some(new AddressingFormSpecifier {
                val modRM = Some(ModRM(DisplacementByte, reg = Some(op1), rm = base))
                val (sib, displacment, immediate) = (None, offset, None)
              })
            case (Some(base), None, None) =>
              Some(new AddressingFormSpecifier {
                val modRM = Some(ModRM(NoDisplacement, reg = Some(op1), rm = base))
                val (sib, displacment, immediate) = (None, None, None)
              })
          }
        case reg: GPR =>
          Some(new AddressingFormSpecifier {
            val modRM = Some(ModRM(TwoRegisters, reg = Some(op1), rm = reg))
            val (sib, displacment, immediate) = (None, None, None)
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
          (mem.base, mem.offset, mem.immediate, opcode.opcodeExtension.isDefined) match {
            case (Some(base), offset @ Some(_: Displacement8), None, true) =>
              Some(new AddressingFormSpecifier {
                val modRM = Some(ModRM(DisplacementByte, opEx = opcode.opcodeExtension, rm = base))
                val (sib, displacment, immediate) = (None, offset, None)
              })
            case (Some(base), None, None, true) =>
              Some(new AddressingFormSpecifier {
                val modRM = Some(ModRM(NoDisplacement, opEx = opcode.opcodeExtension, rm = base))
                val (sib, displacment, immediate) = (None, None, None)
              }) 
            case (None, None, imm @ Some(_: Immediate), true) =>
              Some(new AddressingFormSpecifier {
                val modRM = Some(ModRM(NoDisplacement, opEx = opcode.opcodeExtension, rm = new Displacement32))
                val sib = None
                val (displacment, immediate) = (None, imm)
              })
            case _ => None
          }
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