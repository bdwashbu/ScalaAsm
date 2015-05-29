package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands.Constant
import com.scalaAsm.x86.Operands._

protected[x86] abstract class AddressingFormSpecifier(modRM: ModRM, sib: SIB, displacement: Array[Byte]) {
  import scala.language.postfixOps
  
  def components: Seq[InstructionField] = Seq(modRM, sib)

  lazy val getBytes: Array[Byte] = {
    (components flatMap (_.getBytes) toArray) ++: displacement
  }

  lazy val size: Int = {
    components.flatMap (x => List(x.size)).sum + displacement.size
  }
}

protected[x86] case class InstructionFormat (
  
  addressingForm: AddressingFormSpecifier,
  immediate: Array[Byte]) {
  
  lazy val getBytes: Array[Byte] = {
    addressingForm.getBytes ++: immediate
  }
}

case class NoModRM() extends AddressingFormSpecifier(NoModField, NoSibField, Array())

case class OnlyDisplacement(offset: Array[Byte]) extends AddressingFormSpecifier(NoModField, NoSibField, offset)

case class OnlyModRM(mod: ModRM) extends AddressingFormSpecifier(mod, NoSibField, Array())

case class WithSIBNoDisplacement(mod: ModRM, theSIB: SIB) extends AddressingFormSpecifier(mod, theSIB, Array())

case class NoSIBWithDisplacement(mod: ModRM, offset: Array[Byte]) extends AddressingFormSpecifier(mod, NoSibField, offset)

case class WithSIBWithDisplacement[Mod <: ModRM, Sib <: SIB](mod: Mod, theSIB: Sib, offset: Array[Byte]) extends AddressingFormSpecifier(mod, theSIB, offset)

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

abstract class NoDisp(value: Int) extends Constant[_32](value) {
  override def size: Int = 0
}

case class ModRMReg[X <: GPR, Y <: GPR](mod: RegisterMode, reg: X, rm: Y) extends ModRegisterMemory {
  def getBytes = Array(((mod.value << 6) + (reg.ID << 3) + rm.ID).toByte)
}

// Alternative Mod/RM format
// +---+---+---+---+---+---+---+---+
// |  mod  |Op Extended|     rm    |
// +---+---+---+---+---+---+---+---+

case class ModRMOpcode[X <: GPR](mod: RegisterMode, opcodeExtended: Byte, rm: X) extends ModRegisterMemory {
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

case class ScaleIndexByte[X <: GPR, Y <: GPR](scale: SIBScale, index: X, base: Y) extends SIB {
  def getBytes = Array(((scale.value << 6) + (index.ID << 3) + base.ID).toByte)
  val size = 1
}
