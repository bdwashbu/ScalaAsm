package com.scalaAsm.x86

import com.scalaAsm.utils.Endian
import x86Registers._
import Addressing._

protected[x86] trait AddressingFormSpecifier extends ModRM with Operands {
    val modRM: ModRMFormat
    val scaleIndexBase: Option[Immediate]
    val displacment: Option[Immediate]
    val immediate: Option[Immediate]
    
    def getBytes: Array[Byte] = {
      val components = List(scaleIndexBase, displacment, immediate)
      Array(modRM.get) ++ components.flatten.flatMap(_.getBytes)
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
  
  case class ModRMByte(mod: RegisterMode, reg: Register, rm: Register) extends ModRMFormat {
    def get: Byte = ((mod.value << 6) + (reg.ID << 3) + rm.ID).toByte
  }
  
  case class ModRMByteExtended(mod: RegisterMode, opEx: Byte, rm: Register) extends ModRMFormat {
    def get: Byte = ((mod.value << 6) + (opEx << 3) + rm.ID).toByte
  }

trait ModRM {
  self: Operands =>

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
      if (x.offset8.value != 0 && x.isMemory)
        new AddressingFormSpecifier {
	        val modRM = ModRMByteExtended(Displacment8, opcodeExtension, x.reg)
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = Some(x.offset8)
	    }
      else if (x.offset8.value == 0 && x.isMemory)
        new AddressingFormSpecifier {
	        val modRM = ModRMByteExtended(NoDisplacment, opcodeExtension, x.reg)
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = None
	    }
      else
        new AddressingFormSpecifier {
	        val modRM = ModRMByteExtended(Register, opcodeExtension, x.reg)
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = if (x.offset8.value == 0) None else Some(x.offset8)
	    }
    }
  }
  
  implicit object mod16 extends MODRM_1Extended[rm16] {
    def get(x: rm16, opcodeExtension: Byte) = {
     if (x.offset8.value != 0 && x.isMemory)
        new AddressingFormSpecifier {
	        val modRM = ModRMByteExtended(Displacment8, opcodeExtension, x.reg)
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = Some(x.offset8)
	    }
      else if (x.offset8.value == 0 && x.isMemory)
        new AddressingFormSpecifier {
	        val modRM = ModRMByteExtended(NoDisplacment, opcodeExtension, x.reg)
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = None
	    }
      else
        new AddressingFormSpecifier {
	        val modRM = ModRMByteExtended(Register, opcodeExtension, x.reg)
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = if (x.offset8.value == 0) None else Some(x.offset8)
	    }
    }
  }
  
  implicit object mod26 extends MODRM_1Extended[rm8] {
    def get(x: rm8, opcodeExtension: Byte) = {
      if (x.offset8.value != 0 && x.isMemory)
        new AddressingFormSpecifier {
	        val modRM = ModRMByteExtended(Displacment8, opcodeExtension, x.reg)
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = Some(x.offset8)
	    }
      else if (x.offset8.value == 0 && x.isMemory)
        new AddressingFormSpecifier {
	        val modRM = ModRMByteExtended(NoDisplacment, opcodeExtension, x.reg)
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = None
	    }
      else
        new AddressingFormSpecifier {
	        val modRM = ModRMByteExtended(Register, opcodeExtension, x.reg)
		    val scaleIndexBase = None
		    val displacment = None
		    val immediate = if (x.offset8.value == 0) None else Some(x.offset8)
	    }
    }
  }

  implicit object mod14 extends MODRM_2Extended[r16, imm8] {
    def get(x: r16, y: imm8, opcodeExtension: Byte) = 
      new AddressingFormSpecifier {
	     val modRM = ModRMByteExtended(Register, opcodeExtension, x)
		 val scaleIndexBase = None
		 val displacment = None
		 val immediate = if (y.value == 0) None else Some(y)
	  }
  }
  
  implicit object mod7 extends MODRM_2Extended[r32, imm8] {
    def get(x: r32, y: imm8, opcodeExtension: Byte) = 
      new AddressingFormSpecifier {
	     val modRM = ModRMByteExtended(Register, opcodeExtension, x)
		 val scaleIndexBase = None
		 val displacment = None
		 val immediate = if (y.value == 0) None else Some(y)
	  }
  }

  implicit object mod10 extends MODRM_2Extended[r32, imm32] {
    def get(x: r32, y: imm32, opcodeExtension: Byte) = 
      new AddressingFormSpecifier {
	     val modRM = ModRMByteExtended(Register, opcodeExtension, x)
		 val scaleIndexBase = None
		 val displacment = None
		 val immediate = if (y.value == 0) None else Some(y)
	  }
  }

  implicit object mod3 extends MODRM_2[rm32, r32] {
    def get(x: rm32, y: r32) = mod4.get(y, x)
  }
  
  implicit object mod4 extends MODRM_2[r32, rm32] {
    def get(x: r32, y: rm32) = {
      if (y.reg.ID == 4 && y.offset8.value != 0 && y.isMemory) // [--][--] SIB  
        new AddressingFormSpecifier {
	       val modRM = ModRMByte(Displacment8, x.reg, y.reg)
		   val scaleIndexBase = Some(Immediate8(0x24.toByte))
		   val displacment = None
		   val immediate = Some(y.offset8)
	    }
      else if (y.offset8.value != 0 && y.isMemory)
        new AddressingFormSpecifier {
	       val modRM = ModRMByte(Displacment8, x, y.reg)
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = Some(y.offset8)
	    }
      else if (y.isMemory && y.offset8.value == 0)
        new AddressingFormSpecifier {
	       val modRM = ModRMByte(NoDisplacment, x, y.reg)
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = None
	    }
      else
        new AddressingFormSpecifier {
	       val modRM = ModRMByte(Register, x, y.reg)
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = if (x.offset8.value == 0) None else Some(x.offset8)
	    }
    }
  }
  
  implicit object mod20 extends MODRM_2[r16, rm16] {
    def get(x: r16, y: rm16) = {
      if (y.reg.ID == 4 && y.offset8.value != 0 && y.isMemory) // [--][--] SIB  
        new AddressingFormSpecifier {
	       val modRM = ModRMByte(Displacment8, x, y.reg)
		   val scaleIndexBase = Some(Immediate8(0x24.toByte))
		   val displacment = None
		   val immediate = Some(y.offset8)
	    }
      else if (y.offset8.value != 0 && y.isMemory)
        new AddressingFormSpecifier {
	       val modRM = ModRMByte(Displacment8, x, y.reg)
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = Some(y.offset8)
	    }
      else if (y.isMemory && y.offset8.value == 0)
        new AddressingFormSpecifier {
	       val modRM = ModRMByte(NoDisplacment, x, y.reg)
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = None
	    }
      else
        new AddressingFormSpecifier {
	       val modRM = ModRMByte(Register, x, y.reg)
		   val scaleIndexBase = None
		   val displacment = None
		   val immediate = if (x.offset8.value == 0) None else Some(x.offset8)
	    }
    }
  }
}