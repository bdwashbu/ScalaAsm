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
  def promotedByRex: Boolean
  def x87Only: Boolean
  def code: String
}

// These operandTypes depend on the @opsize field
sealed case class StandandOperandType(val code: String, val name: String, val promotedByRex: Boolean, val x87Only: Boolean) extends OperandType

// These operandTypes depend on the @opsize field
sealed case class CompositeOperandType(val code: String, val name: String, val sizes: Seq[OperandSize], val promotedByRex: Boolean, val x87Only: Boolean) extends OperandType {
   override def toString = sizes mkString 
}

// These operandTypes depend on the @opsize field
sealed case class SizedOperandType(val code: String, val name: String, opsize: Int, val promotedByRex: Boolean, val x87Only: Boolean) extends OperandType

// These are fixed
sealed case class FixedOperandType(val code: String, val name: String, size: OperandSize, promotedByRex: Boolean, x87Only: Boolean) extends OperandType

object OperandType {
  def decodeOperandType(entry: NodeSeq): Seq[OperandType] = {
    val opsize = if ((entry \@ "opsize") == "") 0 else (entry \@ "opsize").toInt

    Seq(CompositeOperandType("a", "Two 16 or 32 Byte Operands", Seq(_16_16, _32_32), false, false),
        FixedOperandType("b", "Byte Operand", _8, false, false),
        StandandOperandType("bcd", "Packed BCD", false, false),
        CompositeOperandType("bs", "Byte Sign Extended To Dst Op", Seq(_8), false, false),
        StandandOperandType("bsq", "Byte Sign Extended To 64", false, false),
        CompositeOperandType("bss", "Byte Sign Extended To StackPtr", Seq(_8), false, false),
        StandandOperandType("c","Byte Or Word", false, false),
        FixedOperandType("d", "Doubleword", _32, false, false),
        FixedOperandType("di", "Doubleword Int", _32, false, false),
        FixedOperandType("dq", "Double Quadword", _128, false, false),
        CompositeOperandType("dqp", "Double Or Quadword", Seq(_32, _64), true, false),
        FixedOperandType("dr", "Double Real", _64, false, false),
        FixedOperandType("ds","Doubleword Sign Extended To 64", _64, false, false),
        StandandOperandType("e", "X87 FPU Environment", false, false),
        StandandOperandType("er", "Extended Real", false, false),
        SizedOperandType("p", "Thirty Two Or 48 Bit Pointer", opsize, false, false),
        FixedOperandType("pi", "Quadword MMX", _64, false, false),
        FixedOperandType("pd", "Bit Packed 128 Double Precision Float", _16, false, false),
        FixedOperandType("ps", "Bit Packed 128 Single Precision Float", _16, false, false),
        FixedOperandType("psq", "Bit Packed 64 Single Precision Float", _8, false, false),
        StandandOperandType("ptp", "Thirty Two Or 48 Or80BitPointer", true, false),
        CompositeOperandType("q", "Quadword Regardless", Seq(_64), false, false),
        StandandOperandType("qi", "Quadword Integer", false, false),
        StandandOperandType("qp", "Quadword Promoted", true, false),
        StandandOperandType("s", "Pseudo Descriptor", false, false),
        StandandOperandType("sd", "Scalar Packed Double Precision Float", false, false),
        StandandOperandType("si", "Double Word Integer Register", false, false),
        FixedOperandType("sr", "Single Real", _32, false, true),
        StandandOperandType("ss", "Scalar Packed Single Precision Float", false, false),
        StandandOperandType("st", "X87 FPU State", false, true),
        StandandOperandType("stx", "X87 FPU And SIMD State", false, true),
        StandandOperandType("t", "Ten Byte Far Pointer", false, false),
        CompositeOperandType("v", "Word Or Doubleword", Seq(_16, _32), false, false),
        CompositeOperandType("vds", "Word Or Doubleword or Doubleword Extended To 64", Seq(_16, _32, _32), false, false),
        CompositeOperandType("vq", "Quadword Or Word", Seq(_64, _16), false, false),
        CompositeOperandType("vqp", "Word Or Doubleword Or Quadword", Seq(_16, _32, _64), true, false),
        CompositeOperandType("vs", "Word Or Doubleword Extended To Stack", Seq(_16, _32), true, false),
        FixedOperandType("w", "Word", _16, false, false),
        FixedOperandType("wi", "Word Integer", _16, false, false),
        StandandOperandType("va", "Word Or Doubleword Based On Address Size", false, false),
        StandandOperandType("dqa", "Doubleword Or Quadword Based On Address Size", false, false),
        StandandOperandType("wa", "Word Based On Address Size", false, false),
        StandandOperandType("wo", "Word Based On Operand Size", false, false),
        StandandOperandType("ws", "Word Based On Stack Size", false, false),
        StandandOperandType("da", "Doubleword Based On Address Size", false, false),
        StandandOperandType("do", "Doubleword Based On Operand Size", false, false),
        StandandOperandType("qa", "Quadword Based On Address Size", false, false),
        StandandOperandType("qs", "Quadword Based On Operand Size", false, false))
  }
}

object Register64 extends CompositeOperandType("r64", "Regiser64", Seq(AX, eAX, rAX), false, false)
object Register8 extends CompositeOperandType("r8", "Regiser8", Seq(AL), false, false)