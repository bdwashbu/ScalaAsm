package com.scalaAsm.x86

import x86Registers._

trait OperandFormat {
  def getAddressingForm: Option[AddressingFormSpecifier]
}

trait OperandEncoding {
  self: Instruction =>
  
  class NoOperand extends OperandFormat {
    def getAddressingForm = None
  }
  
  abstract class OneOperandFormat[X](x:X) extends OperandFormat {
     override def toString = x.toString
  }
  
  abstract class TwoOperandsFormat[X,Y](x:X, y:Y) extends OperandFormat {
     override def toString = x.toString + ", " + y.toString
  }
  
  object NA extends NoOperand  
  
  case class MI[M <: ModRM.reg, I <: Immediate](op1: M, op2: I) extends TwoOperandsFormat[M, I](op1, op2) {
    
    def getAddressingForm: Option[AddressingFormSpecifier] = {
        
      Some(new AddressingFormSpecifier {
	     val modRM = Some(ModRMByte(Register, opEx = Some(opcode.opcodeExtension.get), rm = op1))
		 val scaleIndexBase = None
		 val displacment = None
		 val immediate = if (op2.value == 0) None else Some(op2)
	  })
    }
  }
  
  case class RM[R <: ModRM.reg, M <: ModRM.rm](op1: R, op2: M) extends TwoOperandsFormat[R,M](op1, op2) {
    
     def getAddressingForm: Option[AddressingFormSpecifier] = {
      
      op2 match { 
        case mem: Memory =>
      (mem.base.ID, mem.offset) match {
        case (4, Some(Immediate8(offset))) =>
          Some(new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Displacment8, reg = Some(op1), rm = mem.base))
		   val scaleIndexBase = Some(Immediate8(0x24.toByte))
		   val displacment = None
		   val immediate = Some(Immediate8(offset))
	      })
        case (_, Some(Immediate32(offset))) =>
          Some(new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Displacment32, reg = Some(op1), rm = mem.base))
	       val (scaleIndexBase, displacment, immediate) = (None, None, Some(Immediate32(offset)))
	      })
        case (_, Some(offset)) =>
          Some(new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Displacment8, reg = Some(op1), rm = mem.base))
	       val (scaleIndexBase, displacment, immediate) = (None, None, Some(offset))
	      })
        case (_, None) =>
          Some(new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(NoDisplacment, reg = Some(op1), rm = mem.base))
		   val (scaleIndexBase, displacment, immediate) = (None, None, None)
	      })
      }
      case reg: GPR =>
          Some(new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Register, reg = Some(op1), rm = reg))
	       val (scaleIndexBase, displacment, immediate) = (None, None, None)
	     })
      }
    }
    
  }
  
  case class MR[M <: ModRM.rm, R <: ModRM.reg](op1: M, op2: R) extends TwoOperandsFormat[M,R](op1, op2) {
    
     def getAddressingForm: Option[AddressingFormSpecifier] = {
       RM(op2, op1).getAddressingForm
    }
    
  }
  
  case class O[R <: ModRM.reg](op1: R) extends OneOperandFormat[R](op1) {
    def getAddressingForm: Option[AddressingFormSpecifier] = new M(op1).getAddressingForm
  }
  
  case class OI[R <: ModRM.reg, I <: Immediate](op1: R, op2: I) extends TwoOperandsFormat[R,I](op1, op2) {
    
     def getAddressingForm: Option[AddressingFormSpecifier] = {
      Some(new AddressingFormSpecifier {
	     val modRM = None
	     val (scaleIndexBase, displacment, immediate) = (None, None, Some(op2))
	  })
     }
    
  }
  
  case class I[I <: Immediate](op1: I) extends OneOperandFormat[I](op1) {
    
     def getAddressingForm: Option[AddressingFormSpecifier] = {
      Some(new AddressingFormSpecifier {
	     val modRM = None
		 val (scaleIndexBase, displacment, immediate) = (None, None, Some(op1))
	  })
     }
    
  }
  
  case class M[M <: ModRM.rm](op1: M) extends OneOperandFormat[M](op1) {
    
    def getAddressingForm: Option[AddressingFormSpecifier] = {
      
      if (!opcode.opcodeExtension.isDefined) None else
        op1 match {
        case mem: Memory =>
      
	       (mem.offset) match {
	       case (Some(offset)) =>
	         Some(new AddressingFormSpecifier {
		        val modRM = Some(ModRMByte(Displacment8, opEx = Some(opcode.opcodeExtension.get), rm = mem.base))
		        val (scaleIndexBase, displacment, immediate) = (None, None, Some(offset))
		     })
	       case (None) =>
	         Some(new AddressingFormSpecifier {
		        val modRM = Some(ModRMByte(NoDisplacment, opEx = Some(opcode.opcodeExtension.get), rm = mem.base))
			    val (scaleIndexBase, displacment, immediate) = (None, None, None)
		    })
	       }
        case reg: GPR =>
         Some(new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(Register, opEx = Some(opcode.opcodeExtension.get), rm = reg))
	        val (scaleIndexBase, displacment, immediate) = (None, None, None)
	    })
     }
    }
  }
  
  case class M1[M <: ModRM.rm](op1: M) extends OneOperandFormat[M](op1) {
    def getAddressingForm: Option[AddressingFormSpecifier] = M(op1).getAddressingForm
  }
}