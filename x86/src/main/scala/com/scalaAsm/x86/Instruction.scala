package com.scalaAsm.x86

import com.scalaAsm.x86.ModRM._
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.OperandEncoding._

private[x86] trait Instruction extends ModRM {
  def opcode: Opcodes
  val operands: OperandFormat
  
  implicit def toByte(x:Int) = x.toByte
  
  case class MI[M <: ModRM.reg, I <: Immediate](op1: M, op2: I) extends TwoOperands[M, I](op1, op2) {
    
    def getAddressingForm: Option[AddressingFormSpecifier] = {
        
      Some(new AddressingFormSpecifier {
	     val modRM = Some(ModRMByte(Register, opEx = Some(opcode.opcodeExtension.get), rm = op1))
		 val scaleIndexBase = None
		 val displacment = None
		 val immediate = if (op2.value == 0) None else Some(op2)
	  })
    }
  }
  
  case class RM[R <: ModRM.reg, M <: ModRM.rm](op1: R, op2: M) extends TwoOperands[R,M](op1, op2) {
    
     def getAddressingForm: Option[AddressingFormSpecifier] = {
      
      Some((op2.reg.ID, op2.offset, op2.isMemory) match {
        case (4, Some(Immediate8(offset)), true) =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Displacment8, reg = Some(op1.reg), rm = op2.reg))
		   val scaleIndexBase = Some(Immediate8(0x24.toByte))
		   val displacment = None
		   val immediate = Some(Immediate8(offset))
	      }
        case (_, Some(Immediate32(offset)), true) =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Displacment32, reg = Some(op1.reg), rm = op2.reg))
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = Some(Immediate32(offset))
	      }
        case (_, Some(offset), true) =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Displacment8, reg = Some(op1), rm = op2.reg))
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = Some(offset)
	      } 
        case (_, None, true) =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(NoDisplacment, reg = Some(op1), rm = op2.reg))
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = None
	      }
        case _ =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Register, reg = Some(op1), rm = op2.reg))
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate: Option[Immediate] = op1.offset
	     }
      })
    }
    
  }
  
  case class O[R <: ModRM.reg](op1: R) extends OneOperand[R](op1) {
    
    def getAddressingForm: Option[AddressingFormSpecifier] = {
      
       if (!opcode.opcodeExtension.isDefined) None else
       Some((op1.offset, op1.isMemory) match {
       case (Some(offset), true) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(Displacment8, opEx = Some(opcode.opcodeExtension.get), rm = op1.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = Some(offset)
	     }
       case (None, true) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(NoDisplacment, opEx = Some(opcode.opcodeExtension.get), rm = op1.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = None
	    }
       case (offset, false) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(Register, opEx = Some(opcode.opcodeExtension.get), rm = op1.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = offset
	    }
     })
    }
    
  }
  
  case class I[I <: Immediate](op1: I) extends OneOperand[I](op1) {
    
     def getAddressingForm: Option[AddressingFormSpecifier] = {
      Some(new AddressingFormSpecifier {
	     val modRM = None
		 val scaleIndexBase = None
		 val displacment = None
		 val immediate = Some(op1)
	  })
     }
    
  }
  
  case class M[M <: ModRM.rm](op1: M) extends OneOperand[M](op1) {
    
    def getAddressingForm: Option[AddressingFormSpecifier] = {
      
       Some((op1.offset, op1.isMemory) match {
       case (Some(offset), true) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(Displacment8, opEx = Some(opcode.opcodeExtension.get), rm = op1.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = Some(offset)
	     }
       case (None, true) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(NoDisplacment, opEx = Some(opcode.opcodeExtension.get), rm = op1.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = None
	    }
       case (offset, false) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(Register, opEx = Some(opcode.opcodeExtension.get), rm = op1.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = offset
	    }
     })
    }
  }
  
  case class M1[M <: ModRM.rm](op1: M) extends OneOperand[M](op1) {
    def getAddressingForm: Option[AddressingFormSpecifier] = {
      
       Some((op1.offset, op1.isMemory) match {
       case (Some(offset), true) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(Displacment8, opEx = Some(opcode.opcodeExtension.get), rm = op1.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = Some(offset)
	     }
       case (None, true) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(NoDisplacment, opEx = Some(opcode.opcodeExtension.get), rm = op1.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = None
	    }
       case (offset, false) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(Register, opEx = Some(opcode.opcodeExtension.get), rm = op1.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = offset
	    }
     })
    }
  }

  def getAddressingFormExtended2[X, Y](ops: TwoOperands[X, Y], opcodeExtension: Byte)(implicit ev: MODRM_2Extended[X, Y]): AddressingFormSpecifier = {
    modRM2Extended(ops.operand1, ops.operand2, opcodeExtension)
  }

  def getAddressingFormExtended1[X](ops: OneOperand[X], opcodeExtension: Byte)(implicit ev: MODRM_1Extended[X]): AddressingFormSpecifier = {
    modRMExtended(ops.operand1, opcodeExtension)
  }
  
  def getAddressingForm2[X, Y](ops: TwoOperands[X, Y])(implicit ev: MODRM_2[X, Y]): AddressingFormSpecifier = {
    modRM2(ops.operand1, ops.operand2)
  }

  def getAddressingForm1[X](ops: OneOperand[X])(implicit ev: MODRM_1[X]): AddressingFormSpecifier = {
    modRM(ops.operand1)
  }

  def getBytes: Array[Byte] = {
    opcode.get ++ (operands.getAddressingForm match {
      case Some(modRM) => modRM.getBytes
      case _ => Array.emptyByteArray
    })
  }
}

trait Opcodes {
  def get: Array[Byte]
  val opcodeExtension: Option[Byte]
}

case class OneOpcode(operand1:Byte) extends Opcodes {
  def get = Array(operand1)
  val opcodeExtension: Option[Byte] = None
  def / (x: Byte) = new OneOpcode(operand1) { override val opcodeExtension = Some(x) }
}

case class TwoOpcodes(operand1:Byte, operand2:Byte) extends Opcodes {
  def get = Array(operand1, operand2)
  val opcodeExtension: Option[Byte] = None
  def / (x: Byte) = new TwoOpcodes(operand1, operand2) { override val opcodeExtension = Some(x) }
}
