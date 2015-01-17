package com.scalaAsm.x86.Instructions

import java.io._
import com.scalaAsm.x86.OperandTypes._
import com.scalaAsm.x86.AddressingMethods._
import com.scalaAsm.x86.AddressingMethods.AddressingMethod
import com.scalaAsm.x86.OperandTypes.OperandSize
import com.scalaAsm.x86.AddressingMethods.ModRMByteMemoryOnly
import scala.xml._
import java.io.PrintWriter
import scala.collection.mutable.LinkedHashSet
import com.scalaAsm.x86.OperandTypes.{_8, _16, _32}

object GenerateInst {

  trait InstructionInstance {
    protected def getClassHeader(name: String): String
    protected def hasImplicateOperand: Boolean
    protected def getExplicitFormat: Seq[String]
    val mnemonic: String
    def getSize: Int
    def opcode: Int
    def entry: x86Entry
    def getOperand: Option[OperandInstance]
    def description = entry.brief
    
    override def equals(x: Object) = {
      if (x.isInstanceOf[InstructionInstance]) {
        val otherInstance = x.asInstanceOf[InstructionInstance]
        //opcode == otherInstance.opcode && getOperand == otherInstance.getOperand
        generateClass("")(0) == otherInstance.generateClass("")(0)
      } else {
        false
      }
    }
    
    override def hashCode(): Int = {
      generateClass("")(0).hashCode
    }

    def generateClass(name: String): Seq[String] = {

      val header = getClassHeader(name)

      def getOpcodeString = {
        if (entry.hasRegisterInModRM) {
          Seq("def opcode = 0x" + opcode.toHexString.toUpperCase() + " /r\n")
        } else if (entry.opcodeEx.isDefined) {
          Seq("def opcode = 0x" + opcode.toHexString.toUpperCase() + " /+ " + entry.opcodeEx.get + "\n")
        } else {
          Seq("def opcode = 0x" + opcode.toHexString.toUpperCase() + "\n")
        }
      }

      val opcodeString: Seq[String] = getOperand.map { op =>
        op.addressingMethod match {
          case Some(OpcodeSelectsRegister) =>
            val regCode = if (op.operandType.code == "q") "o" else op.operandType.code // for some reason the code for 64-bit is "ro"
            Seq("def opcode = 0x" + opcode.toHexString.toUpperCase() + " + r" + regCode + "\n")
          case _ => getOpcodeString
          //}

        }
      }.getOrElse(getOpcodeString)

      val prefix: Seq[String] = getOperand.map { op =>
        op match {
          case OperandInstance(address, operandType, size, _) =>
            if (operandType.isInstanceOf[FixedOperandType] && operandType.asInstanceOf[FixedOperandType].promotedByRex && op.operandSize.size == 64) {
              Seq("override def prefix = REX.W(true)\n")
            } else {
              Nil
            }
          case _ => Nil
        }
      }.getOrElse(Nil)

      val implicate: Seq[String] = if (entry.syntax.exists { syn => syn.hasImplicate }) {
        Seq("override def hasImplicateOperand = true\n")
      } else {
        Nil
      }
      val footer = "}"
      header +: (Seq(opcodeString, prefix, getExplicitFormat, implicate).flatten.map(x => "  " + x) :+ footer)
    }
  }

  case class x86ZeroOperandInstruction(opcode: Int,
                                       mnemonic: String,
                                       entry: x86Entry) extends InstructionInstance {

    def getOperand = None

    def getClassHeader(name: String): String = {
      val result = "implicit object " + name + " extends " + mnemonic.toUpperCase() + "._0 {\n"
      result
    }

    def hasImplicateOperand: Boolean = false
    def getExplicitFormat = Nil

    def getSize: Int = 0
  }

  case class x86OneOperandInstruction(opcode: Int,
                                      mnemonic: String,
                                      operand: OperandInstance,
                                      entry: x86Entry) extends InstructionInstance {

    def getOperand = Some(operand)

    def getClassHeader(name: String): String = {
      val result = "implicit object " + name + " extends " + mnemonic.toUpperCase() + "._1[" + operand + "] {\n"
      result
    }

    def hasImplicateOperand: Boolean = {
      operand.isImplicate
    }
    
    def getExplicitFormat = {
        operand.addressingMethod match {
          case Some(OpcodeSelectsRegister) =>
            Seq("override def explicitFormat = Some(InstructionFormat(addressingForm = NoModRM(), immediate = None))\n")
          case _ => Nil
        }
    }

    def getSize: Int = {
      val modSize = if (entry.hasModRMByte) 1 else 0
      1 + modSize + operand.operandSize.size / 8
    }
  }

  case class x86TwoOperandInstruction(opcode: Int,
                                      mnemonic: String,
                                      operands: TwoOperandInstance,
                                      entry: x86Entry) extends InstructionInstance {

    def getClassHeader(name: String): String = {
      val result = "implicit object " + name + " extends " + mnemonic.toUpperCase() + "._2[" + operands._1 + ", " + operands._2 + "] {\n"
      result
    }

    def getOperand = Some(operands._1)

    def hasImplicateOperand = false
    
    def getExplicitFormat = {
      if (operands._2.addressingMethod.isDefined && operands._2.addressingMethod.isDefined && operands._2.addressingMethod.get.abbreviation == "rm" && operands._2.operandSize == _32 &&
            operands._1.addressingMethod.get.abbreviation == "r" && operands._1.operandSize == _32) {
        Seq("override def explicitFormat(op1: r32, op2: rm32) = {\n",
             "  if (op2.isInstanceOf[reg]) {\n",
             "    Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))\n",
             "  } else None\n",
             "}\n")
      } else if (operands._1.addressingMethod.isDefined && operands._2.addressingMethod.isDefined && operands._1.addressingMethod.get.abbreviation == "rm" && operands._1.operandSize == _32 &&
            operands._2.addressingMethod.get.abbreviation == "r" && operands._2.operandSize == _32) {
        Seq("override def explicitFormat(op1: rm32, op2: r32) = {\n",
             "  if (op1.isInstanceOf[reg]) {\n",
             "     Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op2, op1.asInstanceOf[reg])), immediate = None))\n",
             "  } else None\n",
             "}\n")
      } else {
        Nil
      }
      
    }

    def getSize: Int = {
      val modSize = if (entry.hasModRMByte) 1 else 0
      1 + modSize + operands._2.operandSize.size / 8
    }
  }

  case class x86InstructionDef(opcode: Int,
                               mnemonic: String,
                               operands: Seq[OperandDef],
                               entry: x86Entry) {
    def getInstances: Seq[InstructionInstance] = {
      if (operands.size == 2) {
        //if (operands(0).operandType.isDefined &&
        //  operands(1).operandType.isDefined) {
        val ops = TwoOperandDef(operands(0), operands(1))
        ops.getInstances.map { instance => x86TwoOperandInstruction(opcode, mnemonic, instance, entry) }
        //} else if (!operands(1).operandType.isDefined) { // implicate
        //  operands(0).getInstances.map{instance => x86OneOperandInstruction(mnemonic + "_" + opcode + "_" + instance, opcode, mnemonic, instance, entry)}
        //} else {
        //  Nil
      } else if (operands.size == 1) {
        operands(0).getInstances.map { instance => x86OneOperandInstruction(opcode, mnemonic, instance, entry) }
      } else if (operands.isEmpty) {
        Seq(x86ZeroOperandInstruction(opcode, mnemonic, entry))
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
                      hasRegisterInModRM: Boolean,
                      hasModRMByte: Boolean,
                      brief: String)

  case class SyntaxDef(mnemonic: String,
                       operands: Seq[OperandDef],
                       hasImplicate: Boolean)

  case class TwoOperandDef(operand1: OperandDef, operand2: OperandDef) {

    def zipSizes(op1Sizes: Seq[(OperandType, OperandSize)], op2Sizes: Seq[(OperandType, OperandSize)]): Seq[TwoOperandInstance] = {
      op1Sizes.zip(op2Sizes).map { x =>
        val op1 = OperandInstance(
          operand1.addressingMethod,
          x._1._1,
          x._1._2,
          operand1.explicitOperandName)
        val op2 = OperandInstance(
          operand2.addressingMethod,
          x._2._1,
          x._2._2,
          operand2.explicitOperandName)
        TwoOperandInstance(op1, op2)
      }
    }

    def getInstances: Seq[TwoOperandInstance] = {
      //if (operand1.operandType.isDefined && operand2.operandType.isDefined) {
        val op1Sizes: Seq[(OperandType, OperandSize)] = operand1.operandType match {
          case Some(CompositeOperandType(_, _, components, _)) => components map { size => (OperandType.decodeOperandType(size), OperandType.decodeOperandType(size).asInstanceOf[FixedOperandType].size) }
          case Some(FixedOperandType(_, _, size, _, _)) => Seq((operand1.operandType.get, size))
          case _ => {
            operand1.addressingMethod match {
              case Some(ModRMByteMemoryOnly) =>
                Seq((null, NoSize))
                
              case _ => Seq()
            }
          }
        }
        val op2Sizes: Seq[(OperandType, OperandSize)] = operand2.operandType match {
          case Some(CompositeOperandType(_, _, components, _)) => components map { size => (OperandType.decodeOperandType(size), OperandType.decodeOperandType(size).asInstanceOf[FixedOperandType].size) }
          case Some(FixedOperandType(_, _, size, _, _)) => Seq((operand2.operandType.get, size))
          case _ => {
            operand2.addressingMethod match {
              case Some(ModRMByteMemoryOnly) =>
                Seq((null, NoSize))
                
              case _ => Seq()
            }
          }
        }

        (op1Sizes.length,
          op2Sizes.length) match {

            case (_, 1) => {
              for {
                (opType, size1) <- op1Sizes
                (opType2, size2) <- op2Sizes
              } yield {
                val op1 = OperandInstance(
                  operand1.addressingMethod,
                  opType,
                  size1,
                  operand1.explicitOperandName)
                val op2 = OperandInstance(
                  operand2.addressingMethod,
                  opType2,
                  size2,
                  operand2.explicitOperandName)
                TwoOperandInstance(op1, op2)
              }
            }
            case (1, _) => {
              for {
                (opType, size1) <- op1Sizes
                (opType2, size2) <- op2Sizes
              } yield {
                val op1 = OperandInstance(
                  operand1.addressingMethod,
                  opType,
                  size1,
                  operand1.explicitOperandName)
                val op2 = OperandInstance(
                  operand2.addressingMethod,
                  opType2,
                  size2,
                  operand2.explicitOperandName)
                TwoOperandInstance(op1, op2)
              }
            }
            case (x, y) if x == y => {
              zipSizes(op1Sizes, op2Sizes)
            }
            case (3, 2) =>
              val padded = op2Sizes :+ op2Sizes.last
              zipSizes(op1Sizes, padded)
            case (2, 3) =>
              val padded = op1Sizes :+ op1Sizes.last
              zipSizes(padded, op2Sizes)
            case _ =>
              Seq()
      }
    }
  }

  case class OperandDef(srcOrDst: String,
                        operandType: Option[OperandType],
                        addressingMethod: Option[AddressingMethod],
                        explicitOperandName: Option[String]) {

    def getInstances: Seq[OperandInstance] = {
      if (operandType.isDefined) {
        val opSizes: Seq[(OperandType, OperandSize)] = operandType match {
          case Some(CompositeOperandType(_, _, sizes, _)) => sizes map { size => (OperandType.decodeOperandType(size), OperandType.decodeOperandType(size).asInstanceOf[FixedOperandType].size) }
          case Some(FixedOperandType(_, _, size, _, _)) => Seq((operandType.get, size))
          case _ => Seq()
        }
        for {
          (optype, size) <- opSizes
        } yield {
          OperandInstance(
            addressingMethod,
            optype,
            size,
            explicitOperandName)
        }

      } else {
        addressingMethod match {
          case Some(ModRMByteMemoryOnly) =>
            Seq(OperandInstance(
              addressingMethod,
              null,
              NoSize,
              explicitOperandName))
          case _ => Seq()
        }
      }
    }
  }

  case class OperandInstance(addressingMethod: Option[AddressingMethod],
                             operandType: OperandType,
                             operandSize: OperandSize,
                             explicitOperandName: Option[String]) {
    override def toString = {
      explicitOperandName.map { name => 
        if (name == "rAX") {
          operandSize.size match {
            case 16 => "AX"
            case 32 => "EAX"
            case 64 => "RAX"
            case _ => "ERROR"
          }
        } else {
          name
        }
      }.getOrElse(addressingMethod.map { addy => addy.toString }.getOrElse("") + operandSize.toString)
    }
    def isImplicate = false
  }

  case class TwoOperandInstance(_1: OperandInstance, _2: OperandInstance)

  def getOptionalBoolean(node: NodeSeq): Option[Boolean] = {
    if (!node.isEmpty) Some(if (node.text == "0") false else true) else None
  }

  def getOptionalInt(node: NodeSeq): Option[Int] = {
    if (!node.isEmpty) Some(node.text.toInt) else None
  }

  def getOptionalString(node: NodeSeq): Option[String] = {
    if (!node.isEmpty) Some(node.text) else None
  }
  
  def isImplicate(node: Node): Boolean = {
    val isDisplayed = if (!(node \ "@displayed").isEmpty) (node \@ "displayed") == "yes" else true
    val hasNoInfo = (node \ "@type").isEmpty && (node \ "@group").isEmpty && (node \ "a").isEmpty && (node \ "t").isEmpty
    !isDisplayed || hasNoInfo
  }

  def parseSyntax(entry: NodeSeq): Seq[SyntaxDef] = {
    (entry \ "syntax").map { syntax =>
      val mnemonic = (syntax \ "mnem").text
      val operands = (syntax \ "_").filter { node => node.label != "mnem" }
      var hasImplicate = false
      val explicitOperands = operands filter{node => !isImplicate(node)}
         
      val ops = explicitOperands map { operand =>
        val hasDetails = !(operand \ "a").isEmpty

        val opType =
          if (!(operand \ "t").isEmpty && (operand \ "t").text.trim != "")
            Some(OperandType.decodeOperandType((operand \ "t").text.trim))
          else if (!(operand \ "@type").isEmpty)
            Some(OperandType.decodeOperandType((operand \ "@type").text.trim))
          else {
            None
          }

        val opAddressing =
          if (!(operand \ "a").isEmpty)
            Some(AddressingMethod.decodeAddressingMethod((operand \ "a").text.trim))
          else if (!(operand \ "@address").isEmpty)
            Some(AddressingMethod.decodeAddressingMethod((operand \ "@address").text.trim))
          else
            None

        val opName = if (!(operand \ "@type").isEmpty || !(operand \ "@address").isEmpty) {
          Some(operand.text)
        } else {
          None
        }
            
        OperandDef(operand.label, opType, opAddressing, opName)
      }

      val hasImplicateOperand = explicitOperands.size != operands.size
      
      SyntaxDef(mnemonic, ops, hasImplicateOperand)
    }
  }

  def parseEntry(entry: NodeSeq): x86Entry = {
    val mode = getOptionalString(entry \ "@mode")
    val opcodeEx = getOptionalInt(entry \ "opcd_ext")
    val opSize = getOptionalBoolean(entry \ "@opsize")
    val direction = getOptionalBoolean(entry \ "@direction")
    val isRegister = (entry \@ "r") == "yes"
    val note = (entry \ "note" \ "brief")
    val brief = if (note.isEmpty) "" else note.text

    val operandDefs = parseSyntax(entry)

    // seems to be pretty simple
    val hasModRMByte = isRegister || opcodeEx.isDefined

    x86Entry(mode, operandDefs, opcodeEx, opSize, direction, isRegister, hasModRMByte, brief)
  }

  def loadXML(): Seq[x86InstructionDef] = {

    val xml = XML.loadFile("x86reference.xml")
    val pri_opcodes = (xml \\ "pri_opcd")

    val opcodes = pri_opcodes.map { pri_opcode =>
      val nonAliasedEntries = (pri_opcode \ "entry").filter { entry => (entry \ "@alias").size == 0 && ((entry \ "proc_start").size == 0 || (entry \ "proc_start")(0).text == "01" || (entry \ "proc_start")(0).text == "10")}
      val opcode = Integer.parseInt(pri_opcode \@ "value", 16)
      x86Opcode(opcode, nonAliasedEntries.map(parseEntry))
    }

    var lastEntry: x86Entry = null

    val result = opcodes.flatMap { op =>
      op.entries.flatMap { entry =>
        entry.syntax.map { syntax =>
          x86InstructionDef(op.opcode, syntax.mnemonic, syntax.operands, entry)
        }
      }
    }
    
    val mnemonicMap = result.groupBy { x => x.mnemonic }
    
    println("Loading XML..." + opcodes.size + " opcodes read..." + mnemonicMap.size + " mnemonics read...")
    
    result
  }

  def outputInstructionFile(mnemonic: String, instructions: LinkedHashSet[InstructionInstance]) = {
    val writer = new PrintWriter("src/main/scala/com/scalaAsm/x86/Instructions/Standard/" + mnemonic + ".scala", "UTF-8");


    // must do this to resolve (rm, r) (r, rm) ambiguous implicit resolution.  A little hacky
    val (low, high) = instructions.partition { inst =>
      inst match {
        case x86TwoOperandInstruction(_, _, operands, _) if operands._1.addressingMethod.isDefined && operands._2.addressingMethod.isDefined =>
          val is64 = operands._2.addressingMethod.get.abbreviation == "rm" && operands._2.operandSize == _64 &&
            operands._1.addressingMethod.get.abbreviation == "r" && operands._1.operandSize == _64
          val is32 = operands._2.addressingMethod.get.abbreviation == "rm" && operands._2.operandSize == _32 &&
            operands._1.addressingMethod.get.abbreviation == "r" && operands._1.operandSize == _32
          val is16 = operands._2.addressingMethod.get.abbreviation == "rm" && operands._2.operandSize == _16 &&
            operands._1.addressingMethod.get.abbreviation == "r" && operands._1.operandSize == _16
          is64 || is32 || is16
        case x86OneOperandInstruction(_, _, operand, _) if operand.addressingMethod.isDefined && operand.addressingMethod.isDefined =>
          val is64 = operand.addressingMethod.get.abbreviation == "rm" && operand.operandSize == _64
          val is32 = operand.addressingMethod.get.abbreviation == "rm" && operand.operandSize == _32
          val is16 = operand.addressingMethod.get.abbreviation == "rm" && operand.operandSize == _16
          is64 || is32 || is16
        case _ => false
      }
    }
    
    val lowInst = low.zipWithIndex
    val highInst = high.zipWithIndex.map{case(inst,index) => (inst, index + lowInst.size)}

    val briefs = instructions.map(inst => inst.entry.brief).toSet.reduce(_ + ", " + _)
    
    writer.println("package com.scalaAsm.x86");
    writer.println("package Instructions");
    writer.println("package Standard");
    writer.println("")
    writer.println("import com.scalaAsm.x86.Operands._")
    writer.println("import com.scalaAsm.x86.Operands.Memory._")
    writer.println("")
    writer.println("object " + mnemonic.toUpperCase() + " extends InstructionDefinition[OneOpcode](\"" + mnemonic + "\") with " + mnemonic.toUpperCase() + "Impl")
    writer.println("")
    
    if (briefs != "") {
      writer.println("// " + briefs + "\n")
    }

    if (!low.isEmpty && !high.isEmpty) {
      writer.println("trait " + mnemonic.toUpperCase() + "Low {")
      val descriptions = Set[String]()
      for ((inst, index) <- lowInst) {
        writer.println(inst.generateClass(mnemonic + "_" + index).map(x => "  " + x).mkString)
        if (inst != low.last)
          writer.println("") 
      }
      writer.println("}\n")

      writer.println("trait " + mnemonic.toUpperCase() + "Impl extends " + mnemonic.toUpperCase() + "Low {")
      for ((inst, index) <- highInst) {
        writer.println(inst.generateClass(mnemonic + "_" + index).map(x => "  " + x).mkString)
        if (inst != high.last)
          writer.println("")
      }
      writer.println("}")
    } else {
      writer.println("trait " + mnemonic.toUpperCase() + "Impl {")
      for ((inst, index) <- instructions.zipWithIndex) {
        writer.println(inst.generateClass(mnemonic + "_" + index).map(x => "  " + x).mkString)
        if (inst != instructions.last)
          writer.println("")
      }
      writer.println("}")
    }
    writer.close();
  }

  def main(args: Array[String]): Unit = {
    try {
      println("Generating x86 instructions...")
      val insts = loadXML().flatMap { x => x.getInstances }
      println(insts.size + " instruction instances generated!")
      for (mnem <- List("ADD", "AND", "DEC", "NOT", "OR", "XOR", "CMP", "SUB", "SHL", "SHR", "INT", "JMP", "TEST", "MUL", "PUSHF", "PUSH", "LEA", "POP", "LEAVE", "RETN", "JZ", "CALL")) {
        val uniqueInst = LinkedHashSet[InstructionInstance]()
        insts.filter(_.mnemonic == mnem).foreach{x => uniqueInst += x}
        outputInstructionFile(mnem, uniqueInst)
      }
      println("Done generating instructions!")
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

}