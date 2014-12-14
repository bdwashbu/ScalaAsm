package com.scalaAsm

import java.io._
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import com.scalaAsm.coff.Coff
import scala.xml.XML
import scala.xml.NodeSeq
import scala.xml.Elem

object ScalaBasic {

  case class x86InstructionDef(opcode: Int,
                               mnemonic: String,
                               operand1: OperandDef,
                               operand2: OperandDef,
                               opcodeEx: Option[Int],
                               details: x86InstructionDetails) {
    override def toString = {
      val first = "implicit object " + mnemonic + "_" + opcode + " extends " + mnemonic.toUpperCase() + "._2[" + operand1 + ", " + operand2 + "] {\n"
      val second = first + "  def opcode = 0x" + opcode + "\n"
      val third = if (operand1.operandType.map(_.promotedByRex).getOrElse(false) || operand2.operandType.map(_.promotedByRex).getOrElse(false)) {
        "  override def prefix = REX.W(true)\n"
      } else ""
      second + third + "}"
    }
  }
                               
  case class x86InstructionDetails(opsize: Option[Boolean], direction: Option[Boolean])

  case class OperandDef(name: Option[String],
                        operandType: Option[OperandType],
                        addressingMethod: Option[AddressingMethod]) {
    override def toString = {
      name getOrElse addressingMethod.get.toString + operandType.toString
    }
  }

  trait IntelOperand
  object r8 extends IntelOperand
  object r16 extends IntelOperand
  object r32 extends IntelOperand
  object r64NoRex extends IntelOperand
  object r64WithRexW extends IntelOperand
  object rm8 extends IntelOperand
  object rm16 extends IntelOperand
  object rm32 extends IntelOperand
  object rm64NoRex extends IntelOperand
  object rm64WithRexW extends IntelOperand

  sealed abstract class AddressingMethod(val abbreviation: String, val hasRMByte: Boolean) {
    override def toString = abbreviation
  }
  object DirectAddress extends AddressingMethod("ptr", false)
  object MemoryAddressedbyAX extends AddressingMethod("m", false)
  object MemoryAddressedbyAXPlusAL extends AddressingMethod("m", false)
  object MemoryAddressedbyDS extends AddressingMethod("m", false)
  object RegFieldSelectsControlRegister extends AddressingMethod("CRn", true)
  object RegFieldSelectsDebugRegister extends AddressingMethod("DRn", true)
  object ModRMByteRegisterOrMemory extends AddressingMethod("r/m", true)
  object ModRMByteX87StackOrMemory extends AddressingMethod("STi/m", true)
  object ModRMByteX87StackRegister extends AddressingMethod("STi", true)
  object FlagsRegister extends AddressingMethod("-", false)
  object RegFieldRegister extends AddressingMethod("r", true)
  object RMFieldRegisterAlways extends AddressingMethod("r", true)
  object ImmediateData extends AddressingMethod("imm", false)
  object RelativeOffset extends AddressingMethod("rel", false)
  object ModRMByteMemoryOnly extends AddressingMethod("m", true)
  object RMFieldMMXRegister extends AddressingMethod("mm", true)
  object NoModRMByteOrSIBWithOffset extends AddressingMethod("moffs", false)
  object RegFieldMMXRegister extends AddressingMethod("mm", true)
  object ModRMByteMMXRegOrMemory extends AddressingMethod("mm/m64", true)
  object ModFieldRegister extends AddressingMethod("r", true)
  object RegFieldSegmentRegister extends AddressingMethod("Sreg", true)
  object StackOperand extends AddressingMethod("-", false)
  object RegFieldTestRegister extends AddressingMethod("TRn", true)
  object RMField128XMM extends AddressingMethod("xmm", true)
  object RegField128XMM extends AddressingMethod("xmm", true)
  object ModRMByte128XXMOrMemory extends AddressingMethod("xmm/m", true)
  object MemoryAddressedbySI extends AddressingMethod("m", false)
  object MemoryAddressedbyDI extends AddressingMethod("m", false)
  object OpcodeSelectsRegister extends AddressingMethod("r", false)

  sealed abstract class OperandType(val abbreviation: String, val promotedByRex: Boolean, val x87Only: Boolean) {
    override def toString = abbreviation
  }
  object Two16or32ByteOperands extends OperandType("16/32&16/32", false, false)
  object ByteOperand extends OperandType("8", false, false)
  object PackedBCD extends OperandType("80dec", false, false)
  object ByteSignExtendedToDstOp extends OperandType("8", false, false)
  object ByteSignExtendedTo64 extends OperandType("-", false, false)
  object ByteSignExtendedToStackPtr extends OperandType("8", false, false)
  object ByteOrWord extends OperandType("c", false, false) // unused
  object DoubleWord extends OperandType("32", false, false)
  object DoubleWordInt extends OperandType("32int", false, false)
  object DoubleQuadword extends OperandType("128", false, false)
  object DoubleOrQuadword extends OperandType("32/64", true, false)
  object DoubleReal extends OperandType("64real", false, false)
  object DoubleWordSignExtendedTo64 extends OperandType("32", false, false)
  object X87FPUEnvironment extends OperandType("14/28", false, false)
  object ExtendedReal extends OperandType("80real", false, false)
  object ThirtyTwoOr48BitPointer extends OperandType("16:16/32", false, false)
  object QuadwordMMX extends OperandType("(64)", false, false)
  object BitPacked128DoublePrecisionFloat extends OperandType("", false, false)
  object BitPacked128SinglePrecisionFloat extends OperandType("(128)", false, false)
  object BitPacked64SinglePrecisionFloat extends OperandType("64", false, false)
  object ThirtyTwoOr48Or80BitPointer extends OperandType("16:16/32/64", true, false)
  object QuadwordRegardless extends OperandType("64", false, false)
  object QuadwordInteger extends OperandType("64int", false, false)
  object QuadwordPromoted extends OperandType("64int", true, false)
  object PseudoDescriptor extends OperandType("", false, false)
  object ScalarPackedDoublePrecisionFloat extends OperandType("-", false, false)
  object DoubleWordIntegerRegister extends OperandType("?", false, false) // unused
  object SingleReal extends OperandType("32real", false, true)
  object ScalarPackedSinglePrecisionFloat extends OperandType("-", false, false)
  object X87FPUState extends OperandType("94/108", false, true)
  object X87FPUAndSIMDState extends OperandType("512", false, true)
  object TenByteFarPointer extends OperandType("-", false, false)
  object WordOrDoubleword extends OperandType("16/32", false, false)
  object WordOrDoublewordOrDoubleWordExtendedTo64 extends OperandType("16/32", false, false)
  object QuadwordOrWord extends OperandType("64/16", false, false)
  object WordOrDoublewordOrQuadword extends OperandType("16/32/64", true, false)
  object WordOrDoublewordExtendedToStack extends OperandType("16/32", true, false)
  object Word extends OperandType("16", false, false)
  object WordInteger extends OperandType("16int", false, false)

  def decodeAddressingMethod(a: String): AddressingMethod = {
    println(a)
    a match {
      case "A"   => DirectAddress
      case "BA"  => MemoryAddressedbyAX
      case "BB"  => MemoryAddressedbyAXPlusAL
      case "BD"  => MemoryAddressedbyDS
      case "C"   => RegFieldSelectsControlRegister
      case "D"   => RegFieldSelectsDebugRegister
      case "E"   => ModRMByteRegisterOrMemory
      case "ES"  => ModRMByteX87StackOrMemory
      case "EST" => ModRMByteX87StackRegister
      case "F"   => FlagsRegister
      case "G"   => RegFieldRegister
      case "H"   => RMFieldRegisterAlways
      case "I"   => ImmediateData
      case "J"   => RelativeOffset
      case "M"   => ModRMByteMemoryOnly
      case "N"   => RMFieldMMXRegister
      case "O"   => NoModRMByteOrSIBWithOffset
      case "P"   => RegFieldMMXRegister
      case "Q"   => ModRMByteMMXRegOrMemory
      case "R"   => ModFieldRegister
      case "S"   => RegFieldSegmentRegister
      case "SC"  => StackOperand
      case "T"   => RegFieldTestRegister
      case "U"   => RMField128XMM
      case "V"   => RegField128XMM
      case "W"   => ModRMByte128XXMOrMemory
      case "X"   => MemoryAddressedbySI
      case "Y"   => MemoryAddressedbyDI
      case "Z"   => OpcodeSelectsRegister
    }
  }

  def decodeOperandType(t: String): OperandType = {
    println(t)
    t match {
      case "a"   => Two16or32ByteOperands
      case "b"   => ByteOperand
      case "bcd" => PackedBCD
      case "bs"  => ByteSignExtendedToDstOp
      case "bsq" => ByteSignExtendedTo64
      case "bss" => ByteSignExtendedToStackPtr
      case "c"   => ByteOrWord
      case "d"   => DoubleWord
      case "di"  => DoubleWordInt
      case "dq"  => DoubleQuadword
      case "dqp" => DoubleOrQuadword
      case "dr"  => DoubleReal
      case "ds"  => DoubleWordSignExtendedTo64
      case "e"   => X87FPUEnvironment
      case "er"  => ExtendedReal
      case "p"   => ThirtyTwoOr48BitPointer
      case "pi"  => QuadwordMMX
      case "pd"  => BitPacked128DoublePrecisionFloat
      case "ps"  => BitPacked128SinglePrecisionFloat
      case "psq" => BitPacked64SinglePrecisionFloat
      case "ptp" => ThirtyTwoOr48Or80BitPointer
      case "q"   => QuadwordRegardless
      case "qi"  => QuadwordInteger
      case "qp"  => QuadwordPromoted
      case "s"   => PseudoDescriptor
      case "sd"  => ScalarPackedDoublePrecisionFloat
      case "si"  => DoubleWordIntegerRegister
      case "sr"  => SingleReal
      case "ss"  => ScalarPackedSinglePrecisionFloat
      case "st"  => X87FPUState
      case "stx" => X87FPUAndSIMDState
      case "t"   => TenByteFarPointer
      case "v"   => WordOrDoubleword
      case "vds" => WordOrDoublewordOrDoubleWordExtendedTo64
      case "vq"  => QuadwordOrWord
      case "vqp" => WordOrDoublewordOrQuadword
      case "vs"  => WordOrDoublewordExtendedToStack
      case "w"   => Word
      case "wi"  => WordInteger
    }
  }

  def getOptionalBoolean(node: NodeSeq): Option[Boolean] = {
    if (!node.isEmpty) Some(if (node.text == "0") false else true) else None
  }

  def getOptionalInt(node: NodeSeq): Option[Int] = {
    if (!node.isEmpty) Some(node.text.toInt) else None
  }

  def getOptionalString(node: NodeSeq): Option[String] = {
    if (!node.isEmpty) Some(node.text) else None
  }

  def loadXML() = {

    val xml = XML.loadFile("x86reference.xml")
    val good = (xml \\ "pri_opcd")//.filter { x => (x \ "entry" \ "syntax" \ "mnem").text == "ADD"}
    
    
    val defs = good.map { inst =>
      val opcode = Integer.parseInt(inst \@ "value", 16)
      println("WHAT " + opcode)
      val mnemonic = (inst \ "entry" \ "syntax" \ "mnem").text
      val opSize = getOptionalBoolean(inst \ "entry" \ "@opsize")
      val direction = getOptionalBoolean(inst \ "entry" \ "@direction")
      val dstOperand = inst \ "entry" \ "syntax" \ "dst"
      val srcOperand = inst \ "entry" \ "syntax" \ "src"
      val doesDstHaveDetails = !(dstOperand \ "a").isEmpty
      val doesSrcHaveDetails = !(srcOperand \ "a").isEmpty
      val dstName = if (!doesDstHaveDetails) Some(dstOperand.text) else None
      val dstType =
        if (!(dstOperand \ "t").isEmpty)
          Some(decodeOperandType((dstOperand \ "t").text))
        else if (!(dstOperand \ "@type").isEmpty)
          Some(decodeOperandType((dstOperand \ "@type").text))
        else
          None

      val dstAddressing =
        if (!(dstOperand \ "a").isEmpty)
          Some(decodeAddressingMethod((dstOperand \ "a").text))
        else
          None

      val srcName = if (!doesSrcHaveDetails) Some(srcOperand.text) else None
      val srcType =
        if (!(srcOperand \ "t").isEmpty) {
          println("really?: " + (srcOperand \ "t").text)
          Some(decodeOperandType((srcOperand \ "t").text))
        } else if (!(srcOperand \ "@type").isEmpty)
          Some(decodeOperandType((srcOperand \ "@type").text))
        else
          None
          
      val srcAddressing =
        if (!(srcOperand \ "a").isEmpty)
          Some(decodeAddressingMethod((srcOperand \ "a").text))
        else
          None

      val dst = OperandDef(dstName, dstType, dstAddressing)
      val src = OperandDef(srcName, srcType, srcAddressing)
      
      val opcodeEx = getOptionalInt(inst \ "entry" \ "opcd_ext")
      val details = x86InstructionDetails(opSize, direction)
      x86InstructionDef(opcode, mnemonic, dst, src, opcodeEx, details)
    }
    defs.foreach(println)
  }

  def main(args: Array[String]): Unit = {
    try {

      loadXML()

      val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
      val assembler = new Assembler {}
      val linker = new Linker {}

      var beginTime = System.nanoTime()
      val helloWorld = assembler.assemble(HelloWorld3).addIcon("scala.ico")

      val exe = linker.link(helloWorld, 0x3000, false, "kernel32.dll", "msvcrt.dll")

      outputStream.write(exe.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStream.close

      val outputStreamSimple = new DataOutputStream(new FileOutputStream("testSimple.exe"));

      beginTime = System.nanoTime()
      val helloWorldSimple = assembler.assemble(HelloWorld).addIcon("scala.ico")
      helloWorldSimple.write("testCoff.obj")
      val exeSimple = linker.link(helloWorldSimple, 0x3000, false, "kernel32.dll", "msvcrt.dll")

      outputStreamSimple.write(exeSimple.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStreamSimple.close

      val outputStream64 = new DataOutputStream(new FileOutputStream("test64.exe"));

      val helloWorld64 = assembler.assemble(HelloWorld2).addIcon("scala.ico")
      val exe64 = linker.link(helloWorld64, 0x3000, true, "kernel32.dll", "msvcrt.dll")

      outputStream64.write(exe64.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStream64.close

      val outputStream2 = new DataOutputStream(new FileOutputStream("test2.exe"));

      val coff = Coff.readCoff("helloWorld32.obj")
      val exe2 = linker.link(coff, 0x3000, false, "kernel32.dll", "msvcrt.dll")

      outputStream2.write(exe2.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStream2.close
      //      println(coff)

    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

}