package com.scalaAsm.x86
package Operands
package Memory

import com.scalaAsm.x86.Instructions.InstructionField

protected[x86] abstract class AddressingFormSpecifier[Mod <: ModRM, Sib <: SIB, Disp <: Constant[_]](modRM: Mod, sib: Sib, displacement: Disp) {
  import scala.language.postfixOps
  
  def components: Seq[InstructionField] = Seq(modRM, sib, displacement)

  lazy val getBytes: Array[Byte] = {
    components flatMap (_.getBytes) toArray
  }

  lazy val size: Int = {
    components flatMap (x => List(x.size)) sum
  }
}

protected[x86] case class InstructionFormat (
  
  addressingForm: AddressingFormSpecifier[_ <: ModRM,_ <: SIB,_ <: Constant[_]],
  immediate: Option[Constant[_]]) {
  
  lazy val getBytes: Array[Byte] = {
    addressingForm.getBytes ++: (immediate match {
      case Some(imm) => imm.getBytes
      case None => Array[Byte]()
    })
  }
}

case class NoModRM() extends AddressingFormSpecifier(NoModField, NoSibField, NoDispField)

case class OnlyDisplacement[Disp <: Constant[_]](offset: Disp) extends AddressingFormSpecifier(NoModField, NoSibField, offset)

case class OnlyModRM[Mod <: ModRM](mod: Mod) extends AddressingFormSpecifier(mod, NoSibField, NoDispField)

case class WithSIBNoDisplacement[Mod <: ModRM, Sib <: SIB](mod: Mod, theSIB: Sib) extends AddressingFormSpecifier(mod, theSIB, NoDispField)

case class NoSIBWithDisplacement[Mod <: ModRM, Disp <: Constant[_]](mod: Mod, offset: Disp) extends AddressingFormSpecifier(mod, NoSibField, offset)

case class WithSIBWithDisplacement[Mod <: ModRM, Sib <: SIB, Disp <: Constant[_]](mod: Mod, theSIB: Sib, offset: Disp) extends AddressingFormSpecifier(mod, theSIB, offset)

object ModRM {
  type rm = RegisterOrMemory[_]
  type reg = GPR with RegisterOrMemory[_]
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

trait ModRM extends InstructionField

trait ModRegisterMemory extends ModRM {
  val mod: RegisterMode
  val rm: GPR
  val size = 1;
}

trait NoMod extends ModRM {
  def getBytes = Array()
}

object NoModField extends NoMod {
  val size = 0
}

trait NoSib extends SIB {
  def getBytes = Array()
}

object NoSibField extends NoSib {
  val size = 0
}

trait NoDisp extends Constant[NoDisp] {
  type Size = ByteOperand
  def value = 0
  def getBytes: Array[Byte] = Array()
  def asInt: Int = 0
  def asLong: Long = 0
  def negate = this
  def size: Int = 0
}

object NoDispField extends NoDisp

case class ModRMReg(mod: RegisterMode, reg: GPR, rm: GPR) extends ModRegisterMemory {
  def getBytes = Array(((mod.value << 6) + (reg.ID << 3) + rm.ID).toByte)
}

// Alternative Mod/RM format
// +---+---+---+---+---+---+---+---+
// |  mod  |Op Extended|     rm    |
// +---+---+---+---+---+---+---+---+

case class ModRMOpcode(mod: RegisterMode, opcodeExtended: Byte, rm: GPR) extends ModRegisterMemory {
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

trait SIB extends InstructionField

case class ScaleIndexByte(scale: SIBScale, index: GPR, base: GPR) extends SIB {
  def getBytes = Array(((scale.value << 6) + (index.ID << 3) + base.ID).toByte)
  val size = 1
}
