package com.scalaAsm

import java.io._
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import com.scalaAsm.coff.Coff
import scala.xml._
import java.io.PrintWriter

object ScalaBasic {

  trait InstructionInstance {
    def generateClass(numSpaces: Int): String
    val mnemonic: String
  }
  
  case class x86OneOperandInstruction(name: String,
                                      opcode: Int,
                                      mnemonic: String,
                                      operand1: OperandInstance,
                                      entry: x86Entry) extends InstructionInstance {
    
    override def generateClass(numSpaces: Int) = {
      val spaces = (1 to numSpaces) map (x => " ") mkString
      val header = spaces + "implicit object " + name + " extends " + mnemonic.toUpperCase() + "._1[" + operand1 + "] {\n"
      
      val isRegister = if (entry.isRegister) " + r" else ""
      val opcodeString = spaces + "  def opcode = 0x" + opcode.toHexString + isRegister + "\n"
      val prefix = if (operand1.operandType.promotedByRex && operand1.operandSize.size == 64) {
        spaces + "  override def prefix = REX.W(true)\n"
      } else ""
      header + opcodeString + prefix
    }
  }
  
  case class x86TwoOperandInstruction(name: String,
                                      opcode: Int,
                                      mnemonic: String,
                                      operands: TwoOperandInstance,
                                      entry: x86Entry) extends InstructionInstance {
    
    override def generateClass(numSpaces: Int) = {
      val spaces = (1 to numSpaces) map (x => " ") mkString
      val header = spaces + "implicit object " + name + " extends " + mnemonic.toUpperCase() + "._2_new[" + operands._1 + ", " + operands._2 + "] {\n"
      
      val isRegister = if (entry.isRegister) " + r" else ""
      val opcodeString = spaces + "  def opcode = 0x" + opcode.toHexString + isRegister + "\n"
      val prefix = if (operands._1.operandType.promotedByRex && operands._1.operandSize.size == 64) {
        spaces + "  override def prefix = REX.W(true)\n"
      } else ""
        val footer = "  }"
      header + opcodeString + prefix + footer
    }
  }
  
  case class x86InstructionDef(opcode: Int,
                               mnemonic: String,
                               operands: Seq[OperandDef],
                               entry: x86Entry,
                               modes: Seq[x86Entry]) {
    def getInstances: Seq[InstructionInstance] = {
      if (operands.size == 2) {
        val ops = TwoOperandDef(operands(0), operands(1))
        ops.getInstances.map{instance => x86TwoOperandInstruction(mnemonic + "_" + opcode + "_" + instance._1 + "_" + instance._2, opcode, mnemonic, instance, entry)}
      } else {
        Nil
      }
    }

  }

  case class x86Opcode(opcode: Int,
                       entries: Seq[x86Entry])

  case class x86Entry(mode: Option[String],
                      syntax: Seq[SyntaxDef],
                      opcodeEx: Option[Int],
                      opsize: Option[Boolean],
                      direction: Option[Boolean],
                      isRegister: Boolean)

  case class SyntaxDef(mnemonic: String,
                       operands: Seq[OperandDef])

  case class TwoOperandDef(operand1: OperandDef, operand2: OperandDef) {
    
    def zipSizes(op1Sizes: Seq[OperandSize], op2Sizes: Seq[OperandSize]): Seq[TwoOperandInstance] = {
      op1Sizes.zip(op2Sizes).map{ x =>
        val op1 = OperandInstance(
                   operand1.addressingMethod,
                   operand1.operandType.get,
                   x._1)
        val op2 = OperandInstance(
                   operand2.addressingMethod,
                   operand2.operandType.get,
                   x._2)
        TwoOperandInstance(op1, op2)
      }
    }
    
    def getInstances: Seq[TwoOperandInstance] = {
      if (operand1.operandType.isDefined && operand2.operandType.isDefined) {
        (operand1.operandType.get.sizes.length,
          operand2.operandType.get.sizes.length) match {

            
            case (_, 1) => {
              for {
                size1 <- operand1.operandType.get.sizes
                size2 <- operand2.operandType.get.sizes
              } yield {
                val op1 = OperandInstance(
                   operand1.addressingMethod,
                   operand1.operandType.get,
                   size1)
                val op2 = OperandInstance(
                   operand2.addressingMethod,
                   operand2.operandType.get,
                   size2)
                TwoOperandInstance(op1, op2)
              }
            }
            case (1, _) => {
              for {
                size1 <- operand1.operandType.get.sizes
                size2 <- operand2.operandType.get.sizes
              } yield {
                val op1 = OperandInstance(
                   operand1.addressingMethod,
                   operand1.operandType.get,
                   size1)
                val op2 = OperandInstance(
                   operand2.addressingMethod,
                   operand2.operandType.get,
                   size2)
                TwoOperandInstance(op1, op2)
              }
            }
            case (x, y) if x == y => {
                zipSizes(operand1.operandType.get.sizes, operand2.operandType.get.sizes) 
            }
            case (3, 2) =>
              val padded = operand2.operandType.get.sizes :+ operand2.operandType.get.sizes.last
              zipSizes(operand1.operandType.get.sizes, padded) 
            case (2, 3) =>
              val padded = operand1.operandType.get.sizes :+ operand1.operandType.get.sizes.last
              zipSizes(padded, operand2.operandType.get.sizes) 
            case _ =>
              println("HERE")
              println(operand1.operandType.get.sizes.length)
              println(operand2.operandType.get.sizes.length)
              Seq()
          }
        
      } else {
         Nil
      }
    }
  }
                       
  case class OperandDef(srcOrDst: String,
                        operandType: Option[OperandType],
                        addressingMethod: Option[AddressingMethod]) {
    override def toString = {
      addressingMethod.get.toString + operandType.get.toString
    }
  }
  
  case class OperandInstance(addressingMethod: Option[AddressingMethod],
                             operandType: OperandType,
                             operandSize: OperandSize) {
    override def toString = {
      if (addressingMethod.isDefined)
        addressingMethod.get.toString + operandSize.toString
      else
        operandSize.toString
    }
  }
  
  case class TwoOperandInstance(_1: OperandInstance, _2: OperandInstance)

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
  
  object rAX extends OperandSize { override def name = "RAX"; def size = 64}
  object eAX extends OperandSize { override def name = "EAX"; def size = 32}
  object AX extends OperandSize { override def name = "AX"; def size = 16}
  object AL extends OperandSize { override def name = "AL"; def size = 8}

  sealed abstract class AddressingMethod(val abbreviation: String, val hasRMByte: Boolean) {
    override def toString = abbreviation
  }
  object DirectAddress extends AddressingMethod("ptr", false)
  object MemoryAddressedbyAX extends AddressingMethod("m", false)
  object MemoryAddressedbyAXPlusAL extends AddressingMethod("m", false)
  object MemoryAddressedbyDS extends AddressingMethod("m", false)
  object RegFieldSelectsControlRegister extends AddressingMethod("CRn", true)
  object RegFieldSelectsDebugRegister extends AddressingMethod("DRn", true)
  case class ModRMByteRegisterOrMemory(val isReg: Boolean) extends AddressingMethod("rm", true)
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

  // sizes must always be ascending in size
  sealed abstract class OperandType(val sizes: Seq[OperandSize], val promotedByRex: Boolean, val x87Only: Boolean) {
    override def toString = sizes mkString
  }
  object Two16or32ByteOperands extends OperandType(Seq(_16_16, _32_32), false, false)
  object ByteOperand extends OperandType(Seq(_8), false, false)
  object PackedBCD extends OperandType(Seq(), false, false) //80dec
  object ByteSignExtendedToDstOp extends OperandType(Seq(_8), false, false)
  object ByteSignExtendedTo64 extends OperandType(Seq(), false, false) //-
  object ByteSignExtendedToStackPtr extends OperandType(Seq(_8), false, false)
  object ByteOrWord extends OperandType(Seq(), false, false) // unused c
  object DoubleWord extends OperandType(Seq(_32), false, false)
  object DoubleWordInt extends OperandType(Seq(), false, false) //32int
  object DoubleQuadword extends OperandType(Seq(_128), false, false)
  object DoubleOrQuadword extends OperandType(Seq(_32, _64), true, false)
  object DoubleReal extends OperandType(Seq(), false, false) //64real
  object DoubleWordSignExtendedTo64 extends OperandType(Seq(_32), false, false)
  object X87FPUEnvironment extends OperandType(Seq(), false, false) //14/28
  object ExtendedReal extends OperandType(Seq(), false, false) //80real
  object ThirtyTwoOr48BitPointer extends OperandType(Seq(), false, false) //"16:16", "16/32"
  object QuadwordMMX extends OperandType(Seq(), false, false) //(64)
  object BitPacked128DoublePrecisionFloat extends OperandType(Seq(), false, false)
  object BitPacked128SinglePrecisionFloat extends OperandType(Seq(), false, false) //(128)
  object BitPacked64SinglePrecisionFloat extends OperandType(Seq(_64), false, false)
  object ThirtyTwoOr48Or80BitPointer extends OperandType(Seq(), true, false) //"16:16", "16:32", "16:64"
  object QuadwordRegardless extends OperandType(Seq(_64), false, false)
  object QuadwordInteger extends OperandType(Seq(), false, false) //64int
  object QuadwordPromoted extends OperandType(Seq(), true, false) //64int
  object PseudoDescriptor extends OperandType(Seq(), false, false)
  object ScalarPackedDoublePrecisionFloat extends OperandType(Seq(), false, false) //-
  object DoubleWordIntegerRegister extends OperandType(Seq(), false, false) // unused ?
  object SingleReal extends OperandType(Seq(), false, true) //32real
  object ScalarPackedSinglePrecisionFloat extends OperandType(Seq(), false, false) //-
  object X87FPUState extends OperandType(Seq(), false, true) //94 108
  object X87FPUAndSIMDState extends OperandType(Seq(), false, true) //512
  object TenByteFarPointer extends OperandType(Seq(), false, false) //-
  object WordOrDoubleword extends OperandType(Seq(_16, _32), false, false)
  object WordOrDoublewordOrDoubleWordExtendedTo64 extends OperandType(Seq(_16, _32, _32), false, false)
  object QuadwordOrWord extends OperandType(Seq(_64, _16), false, false)
  object WordOrDoublewordOrQuadword extends OperandType(Seq(_16, _32, _64), true, false)
  object WordOrDoublewordExtendedToStack extends OperandType(Seq(_16, _32), true, false)
  object Word extends OperandType(Seq(_16), false, false)
  object WordInteger extends OperandType(Seq(), false, false) //16int
  
  object Register64 extends OperandType(Seq(AX,eAX,rAX), false, false)
  object Register8 extends OperandType(Seq(AL), false, false)

  object WordOrDoublewordBasedOnAddressSize extends OperandType(Seq(), false, false) // REP + LOOP
  object DoublewordOrQuadwordBasedOnAddressSize extends OperandType(Seq(), false, false) // REP + LOOP
  object WordBasedOnAddressSize extends OperandType(Seq(), false, false) // JCXZ
  object WordBasedOnOperandSize extends OperandType(Seq(), false, false) // MOVSW
  object WordBasedOnStackSize extends OperandType(Seq(), false, false) // PUSHF + POPF 64-bit
  object DoublewordBasedOnAddressSize extends OperandType(Seq(), false, false) // JECXZ
  object DoublewordBasedOnOperandSize extends OperandType(Seq(), false, false) // MOVSD
  object QuadwordBasedOnAddressSize extends OperandType(Seq(), false, false) // JRCXZ
  object QuadwordBasedOnOperandSize extends OperandType(Seq(), false, false) // PUSHFQ + POPFQ
  
  

  def decodeAddressingMethod(a: String, entry: NodeSeq): AddressingMethod = {
    a match {
      case "A"   => DirectAddress
      case "BA"  => MemoryAddressedbyAX
      case "BB"  => MemoryAddressedbyAXPlusAL
      case "BD"  => MemoryAddressedbyDS
      case "C"   => RegFieldSelectsControlRegister
      case "D"   => RegFieldSelectsDebugRegister
      case "E"   => ModRMByteRegisterOrMemory((entry \@ "r") == "yes")
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
      case "va"  => WordOrDoublewordBasedOnAddressSize
      case "dqa" => DoublewordOrQuadwordBasedOnAddressSize
      case "wa"  => WordBasedOnAddressSize
      case "wo"  => WordBasedOnOperandSize
      case "ws"  => WordBasedOnStackSize
      case "da"  => DoublewordBasedOnAddressSize
      case "do"  => DoublewordBasedOnOperandSize
      case "qa"  => QuadwordBasedOnAddressSize
      case "qs"  => QuadwordBasedOnOperandSize
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

  def parseSyntax(entry: NodeSeq): Seq[SyntaxDef] = {
    (entry \ "syntax").map { syntax =>
      val mnemonic = (syntax \ "mnem").text
      val operands = (syntax \ "_").filter { node => node.label != "mnem" }

      val ops = operands map { operand =>
        val hasDetails = !(operand \ "a").isEmpty
        //val name = if (!hasDetails) Some(operand.text) else None
        val opType =
          if (operand.text == "rAX") {
            Some(Register64)
          } else if (operand.text == "AL") {
            Some(Register8)
          } else if (!(operand \ "t").isEmpty)
            Some(decodeOperandType((operand \ "t").text.trim))
          else if (!(operand \ "@type").isEmpty)
            Some(decodeOperandType((operand \ "@type").text.trim))
          else
            None

        val opAddressing =
          if (!(operand \ "a").isEmpty)
            Some(decodeAddressingMethod((operand \ "a").text.trim, entry))
          else
            None

        OperandDef(operand.label, opType, opAddressing)
      }

      SyntaxDef(mnemonic, ops)
    }
  }

  def parseEntry(entry: NodeSeq): x86Entry = {
    val mode = getOptionalString(entry \ "@mode")
    val opcodeEx = getOptionalInt(entry \ "opcd_ext")
    val opSize = getOptionalBoolean(entry \ "@opsize")
    val direction = getOptionalBoolean(entry \ "@direction")
    val isRegister = (entry \@ "r") == "yes"

    val operandDefs = parseSyntax(entry)

    x86Entry(mode, operandDefs, opcodeEx, opSize, direction, isRegister)
  }

  def loadXML(): Seq[x86InstructionDef] = {

    val xml = XML.loadFile("x86reference.xml")
    val pri_opcodes = (xml \\ "pri_opcd")

    //    val opcodes = for {
    //      pri_opcode <- pri_opcodes
    //      opcode = Integer.parseInt(pri_opcode \@ "value", 16)
    //      entry <- (pri_opcode \ "entry").filter{entry => (entry \ "@alias").size == 0}
    //    } yield x86Opcode(opcode, entry.map(parseEntry))

    val opcodes = pri_opcodes.flatMap { pri_opcode =>
      val nonAliasedEntries = (pri_opcode \ "entry").filter { entry => (entry \ "@alias").size == 0 }
      val opcode = Integer.parseInt(pri_opcode \@ "value", 16)
      nonAliasedEntries.map { entry =>
        x86Opcode(opcode, entry.map(parseEntry))
      }
    }

    var lastEntry: x86Entry = null

    opcodes.flatMap { op =>
      op.entries.sliding(2).flatMap { entry =>

        val result = if (entry.size == 2 && entry(1).mode == Some("e") && entry(0).mode == None) {
          entry(0).syntax.map { syntax =>
            x86InstructionDef(op.opcode, syntax.mnemonic, syntax.operands, entry(0), Seq(entry(1)))
          }
        } else if (entry.size == 2 && entry(1).mode == None && entry(0).mode == None) {
          entry(0).syntax.map { syntax =>
            x86InstructionDef(op.opcode, syntax.mnemonic, syntax.operands, entry(0), Seq())
          }
        } else if (entry.size == 1) {
          entry(0).syntax.map { syntax =>
            x86InstructionDef(op.opcode, syntax.mnemonic, syntax.operands, entry(0), Seq())
          }
        } else {
          Seq()
        }
        result
      }
    }
  }

  def outputInstructionFile(mnemonic: String, instructions: Seq[InstructionInstance]) = {
    val writer = new PrintWriter(mnemonic + ".scala", "UTF-8");

    writer.println("package com.scalaAsm.x86");
    writer.println("package Instructions");
    writer.println("package Standard");
    writer.println("")
    writer.println("import com.scalaAsm.x86.Operands._")
    writer.println("")
    writer.println("object " + mnemonic.toUpperCase() + " extends InstructionDefinition[OneOpcode](\"" + mnemonic + "\") with AddLow")
    writer.println("")
    writer.println("trait AddLow {")
    for (inst <- instructions) {
      writer.println(inst.generateClass(2))
      if (inst != instructions.last)
        writer.println("")
    }
    writer.println("}")
    writer.close();
  }

  def main(args: Array[String]): Unit = {
    try {

      val insts = loadXML().flatMap{x => x.getInstances}
      outputInstructionFile("ADD", insts.filter(_.mnemonic == "ADD"))

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