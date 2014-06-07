package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86.InstructionField

protected[x86] trait AddressingFormSpecifier {
  val modRM: Option[ModRM]
  val sib: Option[SIB]
  val displacement: Option[Constant]
  
  def components: Seq[InstructionField] = Seq(modRM, sib, displacement).flatten

  lazy val getBytes: Array[Byte] = {
    components flatMap (_.getBytes) toArray
  }

  lazy val size: Int = {
    components flatMap (x => List(x.size)) sum
  }
}

protected[x86] case class InstructionFormat (
  
  addressingForm: AddressingFormSpecifier,
  immediate: Option[Constant]) {
  
  lazy val getBytes: Array[Byte] = {
    addressingForm.getBytes ++ (immediate match {
      case Some(imm) => imm.getBytes
      case None => Array[Byte]()
    })
  }

  lazy val size: Int = {
    addressingForm.size + (immediate match {
      case Some(imm) => imm.size
      case None => 0
    })
  }
}

case class NoModRM() extends AddressingFormSpecifier {
  val sib = None
  val displacement = None
  val modRM = None
}

private[Memory] case class OnlyDisplacement(offset: Constant) extends AddressingFormSpecifier {
  val sib = None
  val displacement = Some(offset)
  val modRM = None
}

case class OnlyModRM(mod: ModRM) extends AddressingFormSpecifier {
  val sib = None
  val displacement = None
  val modRM = Some(mod)
}

private[Memory] case class WithSIBNoDisplacement(mod: ModRM, theSIB: SIB) extends AddressingFormSpecifier {
  val sib = Some(theSIB)
  val displacement = None
  val modRM = Some(mod)
}

private[Memory] case class NoSIBWithDisplacement(mod: ModRM, offset: Constant) extends AddressingFormSpecifier {
  val sib = None
  val displacement = Some(offset)
  val modRM = Some(mod)
}

private[Memory] case class WithSIBWithDisplacement(mod: ModRM, theSIB: SIB, offset: Constant) extends AddressingFormSpecifier {
  val sib = Some(theSIB)
  val displacement = Some(offset)
  val modRM = Some(mod)
}

object ModRM {
  type rm = RegisterOrMemory
  type reg = GPR
  type plusRd = rm
}

sealed class RegisterMode(val value: Byte)
case object NoDisplacement    extends RegisterMode(0) // [reg32 + eax*n]
case object DisplacementByte  extends RegisterMode(1) // [disp + reg8 + eax*n]
case object DisplacementDword extends RegisterMode(2) // [disp + reg32 + eax*n]
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
