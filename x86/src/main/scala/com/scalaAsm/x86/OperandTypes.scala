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

object _8 extends OperandSize { def size = 8 }
object _16 extends OperandSize { def size = 16 }
object _32 extends OperandSize { def size = 32 }
object _64 extends OperandSize { def size = 64 }
object _128 extends OperandSize { def size = 128 }
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
  sealed case class StandandOperandType(val name: String, val sizes: Seq[OperandSize], val promotedByRex: Boolean, val x87Only: Boolean) extends
      OperandType

  // These operandTypes depend on the @opsize field
  sealed case class SizedOperandType(val name: String, opsize: Int, val sizes: Seq[OperandSize], val promotedByRex: Boolean, val x87Only: Boolean) extends
      OperandType
  
  // These are fixed
  sealed case class FixedOperandType(val name: String, size: Int, val sizes: Seq[OperandSize], promotedByRex: Boolean, x87Only: Boolean) extends
      OperandType
  
  object OperandType {
  def decodeOperandType(entry: NodeSeq): Map[String, OperandType] = {
    val opsize = if ((entry \@ "opsize") == "") 0 else (entry \@ "opsize").toInt
    
    Map("a" -> SizedOperandType("Two16or32ByteOperands", opsize, Seq(_16_16, _32_32), false, false),
        "b" -> FixedOperandType("ByteOperand", 1, Seq(_8), false, false),
      "bcd" -> StandandOperandType("PackedBCD", Seq(), false, false) ,
       "bs" -> StandandOperandType("ByteSignExtendedToDstOp", Seq(_8), false, false) ,
      "bsq" -> StandandOperandType("ByteSignExtendedTo64", Seq(), false, false),
      "bss" -> StandandOperandType("ByteSignExtendedToStackPtr", Seq(_8), false, false),
       "c"   -> StandandOperandType("ByteOrWord", Seq(), false, false),
       "d"   -> FixedOperandType("DoubleWord", 4, Seq(_32), false, false),
       "di"  -> FixedOperandType("DoubleWordInt", 4, Seq(), false, false),
       "dq"  -> FixedOperandType("DoubleQuadword", 16, Seq(_128), false, false),
      "dqp" -> StandandOperandType("DoubleOrQuadword", Seq(_32, _64), true, false),
      "dr"  -> FixedOperandType("DoubleReal", 8, Seq(), false, false),
      "ds"  -> FixedOperandType("DoubleWordSignExtendedTo64", 8, Seq(_32), false, false),
      "e"   -> StandandOperandType("X87FPUEnvironment", Seq(), false, false),
      "er"  -> StandandOperandType("ExtendedReal", Seq(), false, false),
       "p"  -> SizedOperandType("ThirtyTwoOr48BitPointer", opsize, Seq(), false, false),
       "pi" -> FixedOperandType("QuadwordMMX", 8, Seq(), false, false),
       "pd" -> FixedOperandType("BitPacked128DoublePrecisionFloat", 16, Seq(), false, false),
       "ps" -> FixedOperandType("BitPacked128SinglePrecisionFloat", 16, Seq(), false, false),
       "psq" -> FixedOperandType("BitPacked64SinglePrecisionFloat", 8, Seq(_64), false, false),
      "ptp" -> StandandOperandType("ThirtyTwoOr48Or80BitPointer", Seq(), true, false),
      "q"   -> StandandOperandType("QuadwordRegardless", Seq(_64), false, false),
      "qi"  -> StandandOperandType("QuadwordInteger", Seq(), false, false),
      "qp"  -> StandandOperandType("QuadwordPromoted", Seq(), true, false),
      "s"   -> StandandOperandType("PseudoDescriptor", Seq(), false, false),
      "sd"  -> StandandOperandType("ScalarPackedDoublePrecisionFloat", Seq(), false, false),
      "si"  -> StandandOperandType("DoubleWordIntegerRegister", Seq(), false, false),
      "sr"  -> FixedOperandType("SingleReal", 4, Seq(), false, true),
      "ss"  -> StandandOperandType("ScalarPackedSinglePrecisionFloat", Seq(), false, false),
      "st"  -> StandandOperandType("X87FPUState", Seq(), false, true),
      "stx" -> StandandOperandType("X87FPUAndSIMDState", Seq(), false, true),
      "t"   -> StandandOperandType("TenByteFarPointer", Seq(), false, false),
      "v"   -> SizedOperandType("WordOrDoubleword", opsize, Seq(_16, _32), false, false),
      "vds" -> SizedOperandType("WordOrDoublewordOrDoubleWordExtendedTo64", opsize, Seq(_16, _32, _32), false, false),
      "vq"  -> SizedOperandType("QuadwordOrWord", opsize, Seq(_64, _16), false, false),
      "vqp" -> SizedOperandType("WordOrDoublewordOrQuadword", opsize, Seq(_16, _32, _64), true, false),
      "vs"  -> SizedOperandType("WordOrDoublewordExtendedToStack", opsize, Seq(_16, _32), true, false),
      "w"   -> FixedOperandType("Word", 2, Seq(_16), false, false),
      "wi"  -> FixedOperandType("WordInteger", 2, Seq(), false, false),
      "va"  -> StandandOperandType("WordOrDoublewordBasedOnAddressSize", Seq(), false, false),
      "dqa" -> StandandOperandType("DoublewordOrQuadwordBasedOnAddressSize", Seq(), false, false),
      "wa"  -> StandandOperandType("WordBasedOnAddressSize", Seq(), false, false),
      "wo"  -> StandandOperandType("WordBasedOnOperandSize", Seq(), false, false),
      "ws"  -> StandandOperandType("WordBasedOnStackSize", Seq(), false, false),
      "da"  -> StandandOperandType("DoublewordBasedOnAddressSize", Seq(), false, false),
      "do"  -> StandandOperandType("DoublewordBasedOnOperandSize", Seq(), false, false),
      "qa"  -> StandandOperandType("QuadwordBasedOnAddressSize", Seq(), false, false),
      "qs"  -> StandandOperandType("QuadwordBasedOnOperandSize", Seq(), false, false)
    )
  }}
      
  //case class Two16or32ByteOperands(val opsize: Int) extends SizedOperandType(opsize, Seq(_16_16, _32_32), false, false)
  //object ByteOperand extends FixedOperandType(1, Seq(_8), false, false)
  //object PackedBCD extends StandandOperandType(Seq(), false, false) //80dec
  //object ByteSignExtendedToDstOp extends StandandOperandType(Seq(_8), false, false)
  //object ByteSignExtendedTo64 extends StandandOperandType(Seq(), false, false) //-
//  object ByteSignExtendedToStackPtr extends StandandOperandType(Seq(_8), false, false)
//  object ByteOrWord extends StandandOperandType(Seq(), false, false) // unused c
//  object DoubleWord extends FixedOperandType(4, Seq(_32), false, false)
//  object DoubleWordInt extends FixedOperandType(4, Seq(), false, false) //32int
//  object DoubleQuadword extends FixedOperandType(16, Seq(_128), false, false)
//  object DoubleOrQuadword extends StandandOperandType(Seq(_32, _64), true, false)
//  object DoubleReal extends FixedOperandType(8, Seq(), false, false) //64real
//  object DoubleWordSignExtendedTo64 extends FixedOperandType(8, Seq(_32), false, false)
//  object X87FPUEnvironment extends StandandOperandType(Seq(), false, false) //14/28
//  object ExtendedReal extends StandandOperandType(Seq(), false, false) //80real
//  case class ThirtyTwoOr48BitPointer(val opsize: Int) extends SizedOperandType(opsize, Seq(), false, false) //"16:16", "16/32"
//  object QuadwordMMX extends FixedOperandType(8, Seq(), false, false) //(64)
//  object BitPacked128DoublePrecisionFloat extends FixedOperandType(16, Seq(), false, false)
//  object BitPacked128SinglePrecisionFloat extends FixedOperandType(16, Seq(), false, false) //(128)
//  object BitPacked64SinglePrecisionFloat extends FixedOperandType(8, Seq(_64), false, false)
//  object ThirtyTwoOr48Or80BitPointer extends StandandOperandType(Seq(), true, false) //"16:16", "16:32", "16:64"
//  object QuadwordRegardless extends StandandOperandType(Seq(_64), false, false)
//  object QuadwordInteger extends StandandOperandType(Seq(), false, false) //64int
//  object QuadwordPromoted extends StandandOperandType(Seq(), true, false) //64int
//  object PseudoDescriptor extends StandandOperandType(Seq(), false, false)
//  object ScalarPackedDoublePrecisionFloat extends StandandOperandType(Seq(), false, false) //-
//  object DoubleWordIntegerRegister extends StandandOperandType(Seq(), false, false) // unused ?
//  object SingleReal extends FixedOperandType(4, Seq(), false, true) //32real
//  object ScalarPackedSinglePrecisionFloat extends FixedOperandType(4, Seq(), false, false) //-
//  object X87FPUState extends StandandOperandType(Seq(), false, true) //94 108
//  object X87FPUAndSIMDState extends StandandOperandType(Seq(), false, true) //512
//  object TenByteFarPointer extends StandandOperandType(Seq(), false, false) //-
//  case class WordOrDoubleword(val opsize: Int) extends SizedOperandType(opsize, Seq(_16, _32), false, false)
//  case class WordOrDoublewordOrDoubleWordExtendedTo64(val opsize: Int) extends SizedOperandType(opsize, Seq(_16, _32, _32), false, false)
//  case class QuadwordOrWord(val opsize: Int) extends SizedOperandType(opsize, Seq(_64, _16), false, false)
//  case class WordOrDoublewordOrQuadword(val opsize: Int) extends SizedOperandType(opsize, Seq(_16, _32, _64), true, false)
//  case class WordOrDoublewordExtendedToStack(val opsize: Int) extends SizedOperandType(opsize, Seq(_16, _32), true, false)
//  object Word extends FixedOperandType(2, Seq(_16), false, false)
//  object WordInteger extends FixedOperandType(2, Seq(), false, false) //16int
//  
  object Register64 extends StandandOperandType("Regiser64", Seq(AX,eAX,rAX), false, false)
  object Register8 extends StandandOperandType("Regiser8", Seq(AL), false, false)
//
//  object WordOrDoublewordBasedOnAddressSize extends StandandOperandType(Seq(), false, false) // REP + LOOP
//  object DoublewordOrQuadwordBasedOnAddressSize extends StandandOperandType(Seq(), false, false) // REP + LOOP
//  object WordBasedOnAddressSize extends StandandOperandType(Seq(), false, false) // JCXZ
//  object WordBasedOnOperandSize extends StandandOperandType(Seq(), false, false) // MOVSW
//  object WordBasedOnStackSize extends StandandOperandType(Seq(), false, false) // PUSHF + POPF 64-bit
//  object DoublewordBasedOnAddressSize extends StandandOperandType(Seq(), false, false) // JECXZ
//  object DoublewordBasedOnOperandSize extends StandandOperandType(Seq(), false, false) // MOVSD
//  object QuadwordBasedOnAddressSize extends StandandOperandType(Seq(), false, false) // JRCXZ
//  object QuadwordBasedOnOperandSize extends StandandOperandType(Seq(), false, false) // PUSHFQ + POPFQ