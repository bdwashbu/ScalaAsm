package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._
import java.nio.{ByteBuffer, ByteOrder}

// Instruction Fields:
/*------------------------------------------------------------------------------------------------------------------------------------------\
|*Prefixes | *Mandatory Prefix | *REX Prefix | Opcode Bytes | *ModR/M | *SIB | *Displacement (1,2 or 4 bytes) | *Immediate (1,2 or 4 bytes) |
\------------------------------------------------------------------------------------------------------------------------------------------*/

sealed trait InstructionField {
  def size: Int
}

sealed trait ByteInstructionField extends InstructionField {
  def size: Int
  def getByte: Option[Byte]
}

abstract class RegisterOrMemory[Size: x86Size] extends InstructionField {
  def size = implicitly[x86Size[Size]].size
}

case class Constant[Size: x86Size](val value: Size)(implicit writer: ConstantWriter[Size]) extends InstructionField {
  
  val buffer = ByteBuffer.allocate(size)
  if (size != 0) {
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    writer.write(buffer, value)
  }
  
  def negate: Constant[Size] = new Constant[Size](getNegative) {}
  def size = implicitly[x86Size[Size]].size
  protected def getNegative = implicitly[x86Size[Size]].negate(value)
  def getBytes: Array[Byte] = {
      buffer.array()
  }

  override def toString = value.toString
}

protected[x86] abstract class AddressingFormSpecifier(modRM: ModRM, sib: SIB, displacement: Array[Byte]) {
  import scala.language.postfixOps

  lazy val getBytes: Array[Byte] = {
    Array(modRM.getByte, sib.getByte).flatten ++: displacement
  }

  lazy val size: Int = {
    modRM.size + sib.size + displacement.size
  }
}

protected[x86] case class InstructionFormat (
  
  addressingForm: AddressingFormSpecifier,
  immediate: Array[Byte]) {
  
  lazy val getBytes: Array[Byte] = {
    addressingForm.getBytes ++: immediate
  }
}

final case class NoModRM() extends AddressingFormSpecifier(NoModField, NoSibField, Array())

final case class OnlyDisplacement(offset: Array[Byte]) extends AddressingFormSpecifier(NoModField, NoSibField, offset)

final case class OnlyModRM(mod: ModRM) extends AddressingFormSpecifier(mod, NoSibField, Array())

final case class WithSIBNoDisplacement(mod: ModRM, theSIB: SIB) extends AddressingFormSpecifier(mod, theSIB, Array())

final case class NoSIBWithDisplacement(mod: ModRM, offset: Array[Byte]) extends AddressingFormSpecifier(mod, NoSibField, offset)

final case class WithSIBWithDisplacement[Mod <: ModRM, Sib <: SIB](mod: Mod, theSIB: Sib, offset: Array[Byte]) extends AddressingFormSpecifier(mod, theSIB, offset)

sealed class RegisterMode(val value: Byte)
case object NoDisplacement    extends RegisterMode(0) // [reg32 + eax*n]
case object DisplacementByte  extends RegisterMode(1) // [disp + reg8 + eax*n]
case object DisplacementDword extends RegisterMode(2) // [disp + reg32 + eax*n]
case object TwoRegisters      extends RegisterMode(3) // r/m is treated as a second "reg" field

trait ModRM extends ByteInstructionField

trait ModRegisterMemory extends ModRM {
  val mod: RegisterMode
  val rm: GPR
  val size = 1;
}

trait NoMod extends ModRM {
  def getByte = None
}

object NoModField extends NoMod {
  val size = 0
}

trait NoSib extends SIB {
  def getByte = None
}

object NoSibField extends NoSib {
  val size = 0
}

abstract class NoDisp(value: Int) extends Constant[_32](value) {
  override def size: Int = 0
}

// Mod/RM format
// +---+---+---+---+---+---+---+---+
// |  mod  |    reg    |     rm    |
// +---+---+---+---+---+---+---+---+

final case class ModRMReg[X <: GPR, Y <: GPR](mod: RegisterMode, reg: X, rm: Y) extends ModRegisterMemory {
  def getByte = Some(((mod.value << 6) + (reg.ID << 3) + rm.ID).toByte)
}

// Alternative Mod/RM format
// +---+---+---+---+---+---+---+---+
// |  mod  |Op Extended|     rm    |
// +---+---+---+---+---+---+---+---+

final case class ModRMOpcode[X <: GPR](mod: RegisterMode, opcodeExtended: Byte, rm: X) extends ModRegisterMemory {
  def getByte = Some(((mod.value << 6) + (opcodeExtended << 3) + rm.ID).toByte)
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

trait SIB extends ByteInstructionField

case class ScaleIndexByte[X <: GPR, Y <: GPR](scale: SIBScale, index: X, base: Y) extends SIB {
  def getByte = Some(((scale.value << 6) + (index.ID << 3) + base.ID).toByte)
  val size = 1
}
