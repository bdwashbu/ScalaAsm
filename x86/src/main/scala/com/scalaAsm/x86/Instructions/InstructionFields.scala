package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Operands._
import java.nio.{ByteBuffer, ByteOrder}

// Instruction Fields:
/*------------------------------------------------------------------------------------------------------------------------------------------\
|*Prefixes | *Mandatory Prefix | *REX Prefix | Opcode Bytes | *ModR/M | *SIB | *Displacement (1,2 or 4 bytes) | *Immediate (1,2 or 4 bytes) |
\------------------------------------------------------------------------------------------------------------------------------------------*/

sealed trait SizedInstructionField {
  def size: Int
}

sealed trait ByteInstructionField {
  val size = 1
  def getByte: Byte
}

sealed trait DependantInstructionField {
  def size(modRM: ModRM): Int
}

abstract class RegisterOrMemory[Size: x86Size]

trait Memory[Size] extends RegisterOrMemory[Size] with DependantInstructionField {
  def size(modRm: ModRM) = {
    modRm.mod match {
      case DisplacementByte => 1
      case DisplacementDword => 4
      case _ => 0
    }
  }
}

case class Constant[Size: x86Size](val value: Size)(implicit writer: ConstantWriter[Size]) extends SizedInstructionField {
  
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

protected[x86] abstract class AddressingFormSpecifier(modRM: Option[ModRM], sib: Option[SIB], displacement: Array[Byte]) {
  import scala.language.postfixOps

  lazy val getBytes: Array[Byte] = {
    val what = Array(modRM, sib).flatten.map(_.getByte)
    Array(modRM, sib).flatten.map(_.getByte) ++: displacement
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

final case class NoModRM() extends AddressingFormSpecifier(None, None, Array())

final case class OnlyDisplacement(offset: Array[Byte]) extends AddressingFormSpecifier(None, None, offset)

final case class OnlyModRM(mod: ModRM) extends AddressingFormSpecifier(Some(mod), None, Array())

final case class WithSIBNoDisplacement(mod: ModRM, theSIB: SIB) extends AddressingFormSpecifier(Some(mod), Some(theSIB), Array())

final case class NoSIBWithDisplacement(mod: ModRM, offset: Array[Byte]) extends AddressingFormSpecifier(Some(mod), None, offset)

final case class WithSIBWithDisplacement(mod: ModRM, theSIB: SIB, offset: Array[Byte]) extends AddressingFormSpecifier(Some(mod), Some(theSIB), offset)

sealed class RegisterMode(val value: Byte)
case object NoDisplacement    extends RegisterMode(0) // [reg32 + eax*n]
case object DisplacementByte  extends RegisterMode(1) // [disp + reg8 + eax*n]
case object DisplacementDword extends RegisterMode(2) // [disp + reg32 + eax*n]
case object TwoRegisters      extends RegisterMode(3) // r/m is treated as a second "reg" field

trait ModRM extends ByteInstructionField {
  val mod: RegisterMode
  val rm: GPR
}

abstract class NoDisp(value: Int) extends Constant[_32](value) {
  override def size: Int = 0
}

// Mod/RM format
// +---+---+---+---+---+---+---+---+
// |  mod  |    reg    |     rm    |
// +---+---+---+---+---+---+---+---+

final case class ModRMReg[X <: GPR, Y <: GPR](mod: RegisterMode, reg: X, rm: Y) extends ModRM {
  def getByte = ((mod.value << 6) + (reg.ID << 3) + rm.ID).toByte
}

// Alternative Mod/RM format
// +---+---+---+---+---+---+---+---+
// |  mod  |Op Extended|     rm    |
// +---+---+---+---+---+---+---+---+

final case class ModRMOpcode[X <: GPR](mod: RegisterMode, opcodeExtended: Byte, rm: X) extends ModRM {
  def getByte = ((mod.value << 6) + (opcodeExtended << 3) + rm.ID).toByte
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
  def getByte = ((scale.value << 6) + (index.ID << 3) + base.ID).toByte
}
