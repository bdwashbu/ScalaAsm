package com.scalaAsm.x86
package OperandTypes

import scala.xml._

trait OperandSize {
  def size: Int
  def name: String = null
  override def toString = {
    if (name == null)
      size.toString
    else
      name
  }
}

case object _8 extends OperandSize { def size = 8 }
case object _16 extends OperandSize { def size = 16 }
case object _32 extends OperandSize { def size = 32 }
case object _64 extends OperandSize { def size = 64 }
case object _128 extends OperandSize { def size = 128 }
object _8_8 extends OperandSize { def size = 16 }
object _16_16 extends OperandSize { def size = 32 }
object _32_32 extends OperandSize { def size = 64 }
object _64_64 extends OperandSize { def size = 128 }

object rAX extends OperandSize { override def name = "RAX"; def size = 64 }
object eAX extends OperandSize { override def name = "EAX"; def size = 32 }
object AX extends OperandSize { override def name = "AX"; def size = 16 }
object AL extends OperandSize { override def name = "AL"; def size = 8 }

// sizes must always be ascending in size
sealed abstract class OperandType {
  val name: String
  def sizes: Seq[OperandSize]
  def promotedByRex: Boolean
  def x87Only: Boolean
  override def toString = sizes mkString
}

// These operandTypes depend on the @opsize field
sealed case class StandandOperandType(val name: String, val sizes: Seq[OperandSize], val promotedByRex: Boolean, val x87Only: Boolean) extends OperandType

// These operandTypes depend on the @opsize field
sealed case class SizedOperandType(val name: String, opsize: Int, val sizes: Seq[OperandSize], val promotedByRex: Boolean, val x87Only: Boolean) extends OperandType

// These are fixed
sealed case class FixedOperandType(val name: String, size: Int, val sizes: Seq[OperandSize], promotedByRex: Boolean, x87Only: Boolean) extends OperandType

object OperandType {
  def decodeOperandType(entry: NodeSeq): Map[String, OperandType] = {
    val opsize = if ((entry \@ "opsize") == "") 0 else (entry \@ "opsize").toInt

    Map("a" -> StandandOperandType("Two 16 or 32 Byte Operands", Seq(_16_16, _32_32), false, false),
      "b" -> FixedOperandType("Byte Operand", 1, Seq(_8), false, false),
      "bcd" -> StandandOperandType("Packed BCD", Seq(), false, false),
      "bs" -> StandandOperandType("Byte Sign Extended To Dst Op", Seq(_8), false, false),
      "bsq" -> StandandOperandType("Byte Sign Extended To 64", Seq(), false, false),
      "bss" -> StandandOperandType("Byte Sign Extended To StackPtr", Seq(_8), false, false),
      "c" -> StandandOperandType("Byte Or Word", Seq(), false, false),
      "d" -> FixedOperandType("Doubleword", 4, Seq(_32), false, false),
      "di" -> FixedOperandType("Doubleword Int", 4, Seq(), false, false),
      "dq" -> FixedOperandType("Double Quadword", 16, Seq(_128), false, false),
      "dqp" -> StandandOperandType("Double Or Quadword", Seq(_32, _64), true, false),
      "dr" -> FixedOperandType("Double Real", 8, Seq(), false, false),
      "ds" -> FixedOperandType("Doubleword Sign Extended To 64", 8, Seq(_32), false, false),
      "e" -> StandandOperandType("X87 FPU Environment", Seq(), false, false),
      "er" -> StandandOperandType("Extended Real", Seq(), false, false),
      "p" -> SizedOperandType("Thirty Two Or 48 Bit Pointer", opsize, Seq(), false, false),
      "pi" -> FixedOperandType("Quadword MMX", 8, Seq(), false, false),
      "pd" -> FixedOperandType("Bit Packed 128 Double Precision Float", 16, Seq(), false, false),
      "ps" -> FixedOperandType("Bit Packed 128 Single Precision Float", 16, Seq(), false, false),
      "psq" -> FixedOperandType("Bit Packed 64 Single Precision Float", 8, Seq(_64), false, false),
      "ptp" -> StandandOperandType("Thirty Two Or 48 Or80BitPointer", Seq(), true, false),
      "q" -> StandandOperandType("Quadword Regardless", Seq(_64), false, false),
      "qi" -> StandandOperandType("Quadword Integer", Seq(), false, false),
      "qp" -> StandandOperandType("Quadword Promoted", Seq(), true, false),
      "s" -> StandandOperandType("Pseudo Descriptor", Seq(), false, false),
      "sd" -> StandandOperandType("Scalar Packed Double Precision Float", Seq(), false, false),
      "si" -> StandandOperandType("Double Word Integer Register", Seq(), false, false),
      "sr" -> FixedOperandType("Single Real", 4, Seq(), false, true),
      "ss" -> StandandOperandType("Scalar Packed Single Precision Float", Seq(), false, false),
      "st" -> StandandOperandType("X87 FPU State", Seq(), false, true),
      "stx" -> StandandOperandType("X87 FPU And SIMD State", Seq(), false, true),
      "t" -> StandandOperandType("Ten Byte Far Pointer", Seq(), false, false),
      "v" -> SizedOperandType("Word Or Doubleword", opsize, Seq(_16, _32), false, false),
      "vds" -> SizedOperandType("Word Or Doubleword or Doubleword Extended To 64", opsize, Seq(_16, _32, _32), false, false),
      "vq" -> SizedOperandType("Quadword Or Word", opsize, Seq(_64, _16), false, false),
      "vqp" -> SizedOperandType("Word Or Doubleword Or Quadword", opsize, Seq(_16, _32, _64), true, false),
      "vs" -> SizedOperandType("Word Or Doubleword Extended To Stack", opsize, Seq(_16, _32), true, false),
      "w" -> FixedOperandType("Word", 2, Seq(_16), false, false),
      "wi" -> FixedOperandType("Word Integer", 2, Seq(), false, false),
      "va" -> StandandOperandType("Word Or Doubleword Based On Address Size", Seq(), false, false),
      "dqa" -> StandandOperandType("Doubleword Or Quadword Based On Address Size", Seq(), false, false),
      "wa" -> StandandOperandType("Word Based On Address Size", Seq(), false, false),
      "wo" -> StandandOperandType("Word Based On Operand Size", Seq(), false, false),
      "ws" -> StandandOperandType("Word Based On Stack Size", Seq(), false, false),
      "da" -> StandandOperandType("Doubleword Based On Address Size", Seq(), false, false),
      "do" -> StandandOperandType("Doubleword Based On Operand Size", Seq(), false, false),
      "qa" -> StandandOperandType("Quadword Based On Address Size", Seq(), false, false),
      "qs" -> StandandOperandType("Quadword Based On Operand Size", Seq(), false, false))
  }
}

object Register64 extends StandandOperandType("Regiser64", Seq(AX, eAX, rAX), false, false)
object Register8 extends StandandOperandType("Regiser8", Seq(AL), false, false)