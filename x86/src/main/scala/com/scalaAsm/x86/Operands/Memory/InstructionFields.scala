package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86.InstructionField

protected[x86] trait AddressingFormSpecifier {
  type Displacement32 = EBP
  type AddressInSib = ESP
  type NoneSib = ESP
  
  val modRM: Option[ModRM]
  val sib: Option[SIB]
  
  def components: Seq[InstructionField] = Seq(modRM, sib).flatten

  lazy val getBytes: Array[Byte] = {
    components flatMap (_.getBytes) toArray
  }

  lazy val size: Int = {
    components flatMap (x => List(x.size)) sum
  }
}

protected[x86] case class InstructionConstants (
  
  addressingForm: AddressingFormSpecifier,
  displacement: Option[Displacement],
  immediate: Option[Immediate]) {
  
  def components: Seq[InstructionField] = Seq(displacement, immediate).flatten

  lazy val getBytes: Array[Byte] = {
    val extra: Array[Byte] = components flatMap (_.getBytes) toArray
    val result = addressingForm.getBytes ++ extra
    result
  }

  lazy val size: Int = {
    addressingForm.size + (components flatMap (x => List(x.size)) sum)
  }
}

case class NoModRM() extends AddressingFormSpecifier {
  val sib = None
  val displacment = None
  val immediate = None
  val modRM = None
}

case class NoSIB(mod: ModRM) extends AddressingFormSpecifier {
  val sib = None
  val displacment = None
  val immediate = None
  val modRM = Some(mod)
}

case class WithSIB(mod: ModRM, theSIB: SIB) extends AddressingFormSpecifier {
  val sib = Some(theSIB)
  val displacment = None
  val immediate = None
  val modRM = Some(mod)
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

// Alternative Mod/RM format
// +---+---+---+---+---+---+---+---+
// |  mod  |Op Extended|     rm    |
// +---+---+---+---+---+---+---+---+

case class ModRMOpcode(mod: RegisterMode, opcodeExtended: Byte, rm: GPR) extends ModRM {
  def getBytes = Array(((mod.value << 6) + (opcodeExtended << 3) + rm.ID).toByte)
}

sealed class SIBScale(val value: Byte)

object SIB {
	case object One   extends SIBScale(0)
	case object Two   extends SIBScale(1)
	case object Four  extends SIBScale(2)
	case object Eight extends SIBScale(3)
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
