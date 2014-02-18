package com.scalaAsm.x86

import com.scalaAsm.utils.Endian
import x86Registers._
import Addressing._
import com.scalaAsm.x86.Operands._

protected[x86] trait AddressingFormSpecifier extends ModRM {
    val modRM: Option[ModRMFormat]
    val scaleIndexBase: Option[Immediate]
    val displacment: Option[Immediate]
    val immediate: Option[Immediate]
    
    def getBytes: Array[Byte] = {
      val components = List(scaleIndexBase, displacment, immediate)
      (modRM match {
        case (Some(modRM)) => Array(modRM.get)
        case _ => Array.emptyByteArray
      }) ++ components.flatten.flatMap(_.getBytes)
    }
  }

object ModRM {
  type rm[Z] = RegisterOrMemory {type Size = Z}
  type reg[Z] = Register {type Size = Z}
  
  trait MODRM_2[-O1, -O2] {
    def get(p1: O1, p2: O2): AddressingFormSpecifier
  }

  trait MODRM_1[-O1] {
    def get(p1: O1): AddressingFormSpecifier
  }

  trait MODRM_2Extended[-O1, -O2] {
    def get(p1: O1, p2: O2, opcodeExtension: Byte): AddressingFormSpecifier
  }

  trait MODRM_1Extended[-O1]  {
    def get(p1: O1, opcodeExtension: Byte): AddressingFormSpecifier
  }
}

 sealed class RegisterMode(val value: Byte)
  case object NoDisplacment extends RegisterMode(0) // If r/m is 110, Displacement (16 bits) is address; otherwise, no displacemen
  case object Displacment8 extends RegisterMode(1)  // Eight-bit displacement, sign-extended to 16 bits
  case object Displacment32 extends RegisterMode(2) // 32-bit displacement (example: MOV [BX + SI]+ displacement,al)
  case object Register extends RegisterMode(3)     // r/m is treated as a second "reg" field

trait ModRMFormat {
    def get: Byte
  }
  
  case class ModRMByte(mod: RegisterMode, rm: Register, reg: Option[Register] = None, opEx: Option[Byte] = None) extends ModRMFormat {
    def get: Byte = {
      (reg, opEx) match {
        case (_, Some(opEx)) => ((mod.value << 6) + (opEx << 3) + rm.ID).toByte
        case (_, _) => ((mod.value << 6) + (reg.get.ID << 3) + rm.ID).toByte
      }
      
    }
  }

trait ModRM {

  import ModRM._
  

  def modRM2[O1, O2](p1: O1, p2: O2)(implicit ev: MODRM_2[O1, O2]) = {
    ev.get(p1, p2)
  }

  def modRM[O1](p1: O1)(implicit ev: MODRM_1[O1]) = {
    ev.get(p1)
  }

  def modRM2Extended[O1, O2](p1: O1, p2: O2, extensionCode: Byte)(implicit ev: MODRM_2Extended[O1, O2]) = {
    ev.get(p1, p2, extensionCode)
  }

  def modRMExtended[O1](p1: O1, extensionCode: Byte)(implicit ev: MODRM_1Extended[O1]) = {
    ev.get(p1, extensionCode)
  }

  implicit object mod6 extends MODRM_1Extended[rm32] {
    def get(x: rm32, opcodeExtension: Byte) = {
      (x.offset, x.isMemory) match {
       case (Some(offset), true) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(Displacment8, opEx = Some(opcodeExtension), rm = x.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = Some(offset)
	     }
       case (None, true) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(NoDisplacment, opEx = Some(opcodeExtension), rm = x.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = None
	    }
       case (offset, false) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(Register, opEx = Some(opcodeExtension), rm = x.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = offset
	    }
     }
    }
  }
  
  implicit object mod16 extends MODRM_1Extended[rm16] {
    def get(x: rm16, opcodeExtension: Byte) = {
      
     (x.offset, x.isMemory) match {
       case (Some(offset), true) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(Displacment8, opEx = Some(opcodeExtension), rm = x.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = Some(offset)
	     }
       case (None, true) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(NoDisplacment, opEx = Some(opcodeExtension), rm = x.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = None
	    }
       case (offset, false) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(Register, opEx = Some(opcodeExtension), rm = x.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = offset
	    }
     }
    }
  }
  
  implicit object mod26 extends MODRM_1Extended[rm8] {
    def get(x: rm8, opcodeExtension: Byte) = {
       (x.offset, x.isMemory) match {
       case (Some(offset), true) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(Displacment8, opEx = Some(opcodeExtension), rm = x.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = Some(offset)
	     }
       case (None, true) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(NoDisplacment, opEx = Some(opcodeExtension), rm = x.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = None
	    }
       case (offset, false) =>
         new AddressingFormSpecifier {
	        val modRM = Some(ModRMByte(Register, opEx = Some(opcodeExtension), rm = x.reg))
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = offset
	    }
     }
    }
  }

  implicit object mod14 extends MODRM_2Extended[r16, imm8] {
    def get(x: r16, y: imm8, opcodeExtension: Byte) = 
      new AddressingFormSpecifier {
	     val modRM = Some(ModRMByte(Register, opEx = Some(opcodeExtension), rm = x))
		 val scaleIndexBase = None
		 val displacment = None
		 val immediate = if (y.value == 0) None else Some(y)
	  }
  }
  
  implicit object mod7 extends MODRM_2Extended[r32, imm8] {
    def get(x: r32, y: imm8, opcodeExtension: Byte) = 
      new AddressingFormSpecifier {
	     val modRM = Some(ModRMByte(Register, opEx = Some(opcodeExtension), rm = x))
		 val scaleIndexBase = None
		 val displacment = None
		 val immediate = if (y.value == 0) None else Some(y)
	  }
  }

  implicit object mod10 extends MODRM_2Extended[r32, imm32] {
    def get(x: r32, y: imm32, opcodeExtension: Byte) = 
      new AddressingFormSpecifier {
	     val modRM = Some(ModRMByte(Register, opEx = Some(opcodeExtension), rm = x))
		 val scaleIndexBase = None
		 val displacment = None
		 val immediate = if (y.value == 0) None else Some(y)
	  }
  }

  implicit object modI8 extends MODRM_1[Immediate] {
    def get(x: Immediate) = 
      new AddressingFormSpecifier {
	     val modRM = None
		 val scaleIndexBase = None
		 val displacment = None
		 val immediate = Some(x)
	  }
  }
  
  implicit object mod3 extends MODRM_2[rm32, r32] {
    def get(x: rm32, y: r32) = mod4.get(y, x)
  }
  
  implicit object mod4 extends MODRM_2[r32, rm32] {
    def get(x: r32, y: rm32) = {
      
      (y.reg.ID, y.offset, y.isMemory) match {
        case (4, Some(Immediate8(offset)), true) =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Displacment8, reg = Some(x.reg), rm = y.reg))
		   val scaleIndexBase = Some(Immediate8(0x24.toByte))
		   val displacment = None
		   val immediate = Some(Immediate8(offset))
	      }
        case (_, Some(Immediate32(offset)), true) =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Displacment32, reg = Some(x.reg), rm = y.reg))
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = Some(Immediate32(offset))
	      }
        case (_, Some(offset), true) =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Displacment8, reg = Some(x), rm = y.reg))
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = Some(offset)
	      } 
        case (_, None, true) =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(NoDisplacment, reg = Some(x), rm = y.reg))
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = None
	      }
        case _ =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Register, reg = Some(x), rm = y.reg))
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate: Option[Immediate] = x.offset
	     }
      }
    }
  }
  
  implicit object mod20 extends MODRM_2[r16, rm16] {
    def get(x: r16, y: rm16) = {
      (y.reg.ID, y.offset, y.isMemory) match {
        case (4, Some(Immediate8(offset)), true) =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Displacment8, reg = Some(x.reg), rm = y.reg))
		   val scaleIndexBase = Some(Immediate8(0x24.toByte))
		   val displacment = None
		   val immediate = Some(Immediate8(offset))
	      }
        case (_, Some(Immediate32(offset)), true) =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Displacment32, reg = Some(x.reg), rm = y.reg))
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = Some(Immediate32(offset))
	      }
        case (_, Some(offset), true) =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Displacment8, reg = Some(x), rm = y.reg))
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = Some(offset)
	      } 
        case (_, None, true) =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(NoDisplacment, reg = Some(x), rm = y.reg))
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = None
	      }
        case _ =>
          new AddressingFormSpecifier {
	       val modRM = Some(ModRMByte(Register, reg = Some(x), rm = y.reg))
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate: Option[Immediate] = x.offset
	     }
      }
    }
  }
}