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
  def size(modRM: Byte): Int
}

abstract class RegisterOrMemory[Size: x86Size]

trait Memory[Size] extends RegisterOrMemory[Size] with DependantInstructionField {
  def size(modRm: Byte) = {
    ModRMReg(modRm).mod match {
      case Indirect8 => 1
      case Indirect32 => 4
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
case object Indirect0   extends RegisterMode(0) // e.g [reg]
case object Indirect8  extends RegisterMode(1) // e.g [reg + disp8]
case object Indirect32 extends RegisterMode(2) // e.g [reg + disp32]
case object TwoRegisters      extends RegisterMode(3) // r/m is treated as a second "reg" field
object RegisterMode {
  def apply(value: Byte) = value match {
    case 0 => Indirect0
    case 1 => Indirect8
    case 2 => Indirect32
    case 3 => TwoRegisters
  }
}

trait ModRM extends ByteInstructionField {
  val mod: RegisterMode
  val rm: Byte
}

abstract class NoDisp(value: Int) extends Constant[_32](value) {
  override def size: Int = 0
}

// Mod/RM format
// * The ModR/M byte encodes a register or an opcode extension, and a register or a memory address

// +---+---+---+---+---+---+---+---+
// |  mod  |    reg    |     rm    |
// +---+---+---+---+---+---+---+---+

final case class ModRMReg(mod: RegisterMode, reg: Byte, rm: Byte) extends ModRM {
  def getByte = ((mod.value << 6) + (reg << 3) + rm).toByte
}

object ModRMReg {
  def apply(mod: Byte): ModRMReg = {
    ModRMReg(RegisterMode((mod >> 6).toByte), ((mod >> 3) & 0x07).toByte, (mod & 0x07).toByte)
  }
}

// Alternative Mod/RM format
// +---+---+---+---+---+---+---+---+
// |  mod  |Op Extended|     rm    |
// +---+---+---+---+---+---+---+---+

final case class ModRMOpcode(mod: RegisterMode, opcodeExtended: Byte, rm: Byte) extends ModRM {
  def getByte = ((mod.value << 6) + (opcodeExtended << 3) + rm).toByte
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
