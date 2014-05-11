package com.scalaAsm.x86

import com.scalaAsm.utils.Endian
import Operands._

protected[x86] trait AddressingFormSpecifier {
  type Displacement32 = EBP
  type AddressInSib = ESP
  type NoneSib = ESP
  
  val modRM: Option[ModRM]
  val sib: Option[SIB]
  val displacment: Option[Displacement]
  val immediate: Option[Immediate]
  
  def components: Seq[InstructionField] = Seq(modRM, sib, displacment, immediate).flatten

  lazy val getBytes: Array[Byte] = {
    components flatMap (_.getBytes) toArray
  }

  lazy val size: Int = {
    components flatMap (x => List(x.size)) sum
    //getBytes.size
  }
}

object ModRM {
  type rm = RegisterOrMemory
  type reg = GPR
  type plusRd = rm
}

sealed class RegisterMode(val value: Byte)
case object NoDisplacement    extends RegisterMode(0) // If r/m is 110, Displacement (16 bits) is address; otherwise, no displacemen
case object DisplacementByte  extends RegisterMode(1) // Eight-bit displacement, sign-extended to 16 bits
case object DisplacementDword extends RegisterMode(2) // 32-bit displacement (example: MOV [BX + SI]+ displacement,al)
case object TwoRegisters      extends RegisterMode(3) // r/m is treated as a second "reg" field

// Mod/RM format
//   7                           0
// +---+---+---+---+---+---+---+---+
// |  mod  |    reg    |     rm    |
// +---+---+---+---+---+---+---+---+

trait ModRM extends InstructionField {
  val mod: RegisterMode
  val rm: GPR
  val size = 1;
}

case class ModRMReg(mod: RegisterMode, reg: GPR, rm: GPR) extends ModRM {
  def getBytes = Array(((mod.value << 6) + (reg.ID << 3) + rm.ID).toByte)
}

case class ModRMOpcode(mod: RegisterMode, opcodeExtended: Byte, rm: GPR) extends ModRM {
  def getBytes = Array(((mod.value << 6) + (opcodeExtended << 3) + rm.ID).toByte)
}

sealed class SIBScale(val value: Byte)

object SIB {
	case object One   extends SIBScale(0) // If r/m is 110, Displacement (16 bits) is address; otherwise, no displacement
	case object Two   extends SIBScale(1) // Eight-bit displacement, sign-extended to 16 bits
	case object Four  extends SIBScale(2) // 32-bit displacement (example: MOV [BX + SI]+ displacement,al)
	case object Eight extends SIBScale(3) // r/m is treated as a second "reg" field
}

// Scale-Index-Base (SIB) format
//   7                           0
// +---+---+---+---+---+---+---+---+
// | scale |   index   |    base   |
// +---+---+---+---+---+---+---+---+

case class SIB(scale: SIBScale, index: GPR, base: GPR) extends InstructionField {
  def getBytes = Array(((scale.value << 6) + (index.ID << 3) + base.ID).toByte)
  val size = 1
}
