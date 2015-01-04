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
  def code: String
}

// These operandTypes depend on the @opsize field
sealed case class StandandOperandType(val code: String, val name: String, val sizes: Seq[OperandSize], val promotedByRex: Boolean, val x87Only: Boolean) extends OperandType

// These operandTypes depend on the @opsize field
sealed case class SizedOperandType(val code: String, val name: String, opsize: Int, val sizes: Seq[OperandSize], val promotedByRex: Boolean, val x87Only: Boolean) extends OperandType

// These are fixed
sealed case class FixedOperandType(val code: String, val name: String, size: Int, val sizes: Seq[OperandSize], promotedByRex: Boolean, x87Only: Boolean) extends OperandType

object OperandType {
  def decodeOperandType(entry: NodeSeq): Seq[OperandType] = {
    val opsize = if ((entry \@ "opsize") == "") 0 else (entry \@ "opsize").toInt

    Seq(StandandOperandType("a", "Two 16 or 32 Byte Operands", Seq(_16_16, _32_32), false, false),
        FixedOperandType("b", "Byte Operand", 1, Seq(_8), false, false),
        StandandOperandType("bcd", "Packed BCD", Seq(), false, false),
        StandandOperandType("bs", "Byte Sign Extended To Dst Op", Seq(_8), false, false),
        StandandOperandType("bsq", "Byte Sign Extended To 64", Seq(), false, false),
        StandandOperandType("bss", "Byte Sign Extended To StackPtr", Seq(_8), false, false),
        StandandOperandType("c","Byte Or Word", Seq(), false, false),
        FixedOperandType("d", "Doubleword", 4, Seq(_32), false, false),
        FixedOperandType("di", "Doubleword Int", 4, Seq(), false, false),
        FixedOperandType("dq", "Double Quadword", 16, Seq(_128), false, false),
        StandandOperandType("dqp", "Double Or Quadword", Seq(_32, _64), true, false),
        FixedOperandType("dr", "Double Real", 8, Seq(), false, false),
        FixedOperandType("ds","Doubleword Sign Extended To 64", 8, Seq(_32), false, false),
        StandandOperandType("e", "X87 FPU Environment", Seq(), false, false),
        StandandOperandType("er", "Extended Real", Seq(), false, false),
        SizedOperandType("p", "Thirty Two Or 48 Bit Pointer", opsize, Seq(), false, false),
        FixedOperandType("pi", "Quadword MMX", 8, Seq(), false, false),
        FixedOperandType("pd", "Bit Packed 128 Double Precision Float", 16, Seq(), false, false),
        FixedOperandType("ps", "Bit Packed 128 Single Precision Float", 16, Seq(), false, false),
        FixedOperandType("psq", "Bit Packed 64 Single Precision Float", 8, Seq(_64), false, false),
        StandandOperandType("ptp", "Thirty Two Or 48 Or80BitPointer", Seq(), true, false),
        StandandOperandType("q", "Quadword Regardless", Seq(_64), false, false),
        StandandOperandType("qi", "Quadword Integer", Seq(), false, false),
        StandandOperandType("qp", "Quadword Promoted", Seq(), true, false),
        StandandOperandType("s", "Pseudo Descriptor", Seq(), false, false),
        StandandOperandType("sd", "Scalar Packed Double Precision Float", Seq(), false, false),
        StandandOperandType("si", "Double Word Integer Register", Seq(), false, false),
        FixedOperandType("sr", "Single Real", 4, Seq(), false, true),
        StandandOperandType("ss", "Scalar Packed Single Precision Float", Seq(), false, false),
        StandandOperandType("st", "X87 FPU State", Seq(), false, true),
        StandandOperandType("stx", "X87 FPU And SIMD State", Seq(), false, true),
        StandandOperandType("t", "Ten Byte Far Pointer", Seq(), false, false),
        SizedOperandType("v", "Word Or Doubleword", opsize, Seq(_16, _32), false, false),
        SizedOperandType("vds", "Word Or Doubleword or Doubleword Extended To 64", opsize, Seq(_16, _32, _32), false, false),
        SizedOperandType("vq", "Quadword Or Word", opsize, Seq(_64, _16), false, false),
        SizedOperandType("vqp", "Word Or Doubleword Or Quadword", opsize, Seq(_16, _32, _64), true, false),
        SizedOperandType("vs", "Word Or Doubleword Extended To Stack", opsize, Seq(_16, _32), true, false),
        FixedOperandType("w", "Word", 2, Seq(_16), false, false),
        FixedOperandType("wi", "Word Integer", 2, Seq(), false, false),
        StandandOperandType("va", "Word Or Doubleword Based On Address Size", Seq(), false, false),
        StandandOperandType("dqa", "Doubleword Or Quadword Based On Address Size", Seq(), false, false),
        StandandOperandType("wa", "Word Based On Address Size", Seq(), false, false),
        StandandOperandType("wo", "Word Based On Operand Size", Seq(), false, false),
        StandandOperandType("ws", "Word Based On Stack Size", Seq(), false, false),
        StandandOperandType("da", "Doubleword Based On Address Size", Seq(), false, false),
        StandandOperandType("do", "Doubleword Based On Operand Size", Seq(), false, false),
        StandandOperandType("qa", "Quadword Based On Address Size", Seq(), false, false),
        StandandOperandType("qs", "Quadword Based On Operand Size", Seq(), false, false))
  }
}

object Register64 extends StandandOperandType("r64", "Regiser64", Seq(AX, eAX, rAX), false, false)
object Register8 extends StandandOperandType("r8", "Regiser8", Seq(AL), false, false)