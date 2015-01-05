package com.scalaAsm

import java.io._
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import com.scalaAsm.coff.Coff
import com.scalaAsm.x86.OperandTypes._
import com.scalaAsm.x86.OperandTypes.Register64
import com.scalaAsm.x86.AddressingMethods._
import com.scalaAsm.x86.AddressingMethods.AddressingMethod
import com.scalaAsm.x86.OperandTypes.OperandSize
import scala.xml._
import java.io.PrintWriter

object ScalaBasic {

  trait InstructionInstance {
    def generateClass(numSpaces: Int): String
    val mnemonic: String
    def getSize: Int
  }
  
  case class x86OneOperandInstruction(name: String,
                                      opcode: Int,
                                      mnemonic: String,
                                      operand: OperandInstance,
                                      entry: x86Entry) extends InstructionInstance {
    
    override def generateClass(numSpaces: Int) = {
      
      val spaces = (1 to numSpaces) map (x => " ") mkString
      val header = spaces + "implicit object " + name + " extends " + mnemonic.toUpperCase() + "._1_new[" + operand + "] {\n"
      
      val opcodeString = operand.addressingMethod match {
        case Some(OpcodeSelectsRegister) => operand.operandSize.size match {
          case 8 => spaces + "  def opcode = 0x" + opcode.toHexString.toUpperCase() + " + rb\n"
          case 16 => spaces + "  def opcode = 0x" + opcode.toHexString.toUpperCase() + " + rw\n"
          case 32 => spaces + "  def opcode = 0x" + opcode.toHexString.toUpperCase() + " + rd\n"
        }
        case _ =>
          if (entry.hasRegisterInModRM) {
            spaces + "  def opcode = 0x" + opcode.toHexString.toUpperCase() + " / r\n"
          } else if (entry.opcodeEx.isDefined) {
            spaces + "  def opcode = 0x" + opcode.toHexString.toUpperCase() + " /+ " + entry.opcodeEx.get + "\n"
          } else {
            spaces + "  def opcode = 0x" + opcode.toHexString.toUpperCase() + "\n"
          }
      }
      val prefix = if (operand.operandType.isInstanceOf[FixedOperandType] && operand.operandType.asInstanceOf[FixedOperandType].promotedByRex && operand.operandSize.size == 64) {
        spaces + "  override def prefix = REX.W(true)\n"
      } else ""
        val footer = "  }"
      header + opcodeString + prefix + footer
    }
    
    def getSize: Int = {
      val modSize = if (entry.hasModRMByte) 1 else 0
      1 + modSize + operand.operandSize.size / 8
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

      val opcodeString = if (entry.hasRegisterInModRM) {
        spaces + "  def opcode = 0x" + opcode.toHexString.toUpperCase() + " / r\n"
      } else if (entry.opcodeEx.isDefined) {
        spaces + "  def opcode = 0x" + opcode.toHexString.toUpperCase() + " /+ " + entry.opcodeEx.get + "\n"
      } else {
        spaces + "  def opcode = 0x" + opcode.toHexString.toUpperCase() + "\n"
      }
      val prefix = if (operands._1.operandType.isInstanceOf[FixedOperandType] && operands._1.operandType.asInstanceOf[FixedOperandType].promotedByRex && operands._1.operandSize.size == 64) {
        spaces + "  override def prefix = REX.W(true)\n"
      } else {
        ""
      }
      val footer = "  }"
      header + opcodeString + prefix + footer
    }
    
    def getSize: Int = {
      val modSize = if (entry.hasModRMByte) 1 else 0
      1 + modSize + operands._2.operandSize.size / 8
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
      } else if (operands.size == 1) {
        operands(0).getInstances.map{instance => x86OneOperandInstruction(mnemonic + "_" + opcode + "_" + instance, opcode, mnemonic, instance, entry)}
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
                      hasModRMByte: Boolean)

  case class SyntaxDef(mnemonic: String,
                       operands: Seq[OperandDef])

  case class TwoOperandDef(operand1: OperandDef, operand2: OperandDef) {
    
    def zipSizes(op1Sizes: Seq[(OperandType, OperandSize)], op2Sizes: Seq[(OperandType, OperandSize)]): Seq[TwoOperandInstance] = {
      op1Sizes.zip(op2Sizes).map{ x =>
        val op1 = OperandInstance(
                   operand1.addressingMethod,
                   x._1._1,
                   x._1._2)
        val op2 = OperandInstance(
                   operand2.addressingMethod,
                   x._2._1,
                   x._2._2)
        TwoOperandInstance(op1, op2)
      }
    }
    
    def getInstances: Seq[TwoOperandInstance] = {
      if (operand1.operandType.isDefined && operand2.operandType.isDefined) {
        val op1Sizes: Seq[(OperandType, OperandSize)] = operand1.operandType match {
          case Some(CompositeOperandType(_,_,components,_)) => components map{ size => (OperandType.decodeOperandType(size), OperandType.decodeOperandType(size).asInstanceOf[FixedOperandType].size)}
          case Some(FixedOperandType(_,_,size,_,_)) => Seq((operand1.operandType.get, size))
          case _ => Seq()
        }
        val op2Sizes: Seq[(OperandType, OperandSize)] = operand2.operandType match {
          case Some(CompositeOperandType(_,_,components,_)) => components map{ size => (OperandType.decodeOperandType(size), OperandType.decodeOperandType(size).asInstanceOf[FixedOperandType].size)}
          case Some(FixedOperandType(_,_,size,_,_)) => Seq((operand2.operandType.get, size))
          case _ => Seq()
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
                   size1)
                val op2 = OperandInstance(
                   operand2.addressingMethod,
                   opType2,
                   size2)
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
                   size1)
                val op2 = OperandInstance(
                   operand2.addressingMethod,
                   opType2,
                   size2)
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
              println("HERE")
              println(op1Sizes.length)
              println(op2Sizes.length)
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
    
    def getInstances: Seq[OperandInstance] = {
      if (operandType.isDefined && operandType.isDefined) {
        val opSizes: Seq[(OperandType, OperandSize)] = operandType match {
          case Some(CompositeOperandType(_,_,sizes,_)) => sizes map{ size => (OperandType.decodeOperandType(size), OperandType.decodeOperandType(size).asInstanceOf[FixedOperandType].size)}
          case Some(FixedOperandType(_,_,size,_,_)) => Seq((operandType.get, size))
          case _ => Seq()
        }
          for {
            (optype, size) <- opSizes
          } yield {
            OperandInstance(
               addressingMethod,
               optype,
               size)
          }
        
      } else {
         Nil
      }
    }
  }
  
  case class OperandInstance(addressingMethod: Option[AddressingMethod],
                             operandType: OperandType,
                             operandSize: OperandSize) {
    override def toString = {
      if (addressingMethod.isDefined)
        addressingMethod.get.toString + operandSize.toString
      else
        operandType.toString
    }
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
          } else if (!(operand \ "t").isEmpty && (operand \ "t").text.trim != "")
            Some(OperandType.decodeOperandType((operand \ "t").text.trim))
          else if (!(operand \ "@type").isEmpty)
            Some(OperandType.decodeOperandType((operand \ "@type").text.trim))
          else
            None

        val opAddressing =
          if (!(operand \ "a").isEmpty)
            Some(AddressingMethod.decodeAddressingMethod((operand \ "a").text.trim))
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
    
    // seems to be pretty simple
    val hasModRMByte = isRegister || opcodeEx.isDefined

    x86Entry(mode, operandDefs, opcodeEx, opSize, direction, isRegister, hasModRMByte)
  }

  def loadXML(): Seq[x86InstructionDef] = {

    val xml = XML.loadFile("x86reference.xml")
    val pri_opcodes = (xml \\ "pri_opcd")

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
    
    // must do this to resolve (rm, r) (r, rm) ambiguous implicit resolution.  A little hacky
    val (low, high) = instructions.partition { inst => inst match {
      case x86TwoOperandInstruction(_,_,_,operands,_) if operands._1.addressingMethod.isDefined && operands._2.addressingMethod.isDefined =>
        val is64 = operands._2.addressingMethod.get.abbreviation == "rm" && operands._2.operandSize == _64 &&
            operands._1.addressingMethod.get.abbreviation == "r" && operands._1.operandSize == _64
        val is32 = operands._2.addressingMethod.get.abbreviation == "rm" && operands._2.operandSize == _32 &&
            operands._1.addressingMethod.get.abbreviation == "r" && operands._1.operandSize == _32
        val is16 = operands._2.addressingMethod.get.abbreviation == "rm" && operands._2.operandSize == _16 &&
            operands._1.addressingMethod.get.abbreviation == "r" && operands._1.operandSize == _16
        is64 || is32 || is16
      case _ => false
       }
    }

    writer.println("package com.scalaAsm.x86");
    writer.println("package Instructions");
    writer.println("package Standard");
    writer.println("")
    writer.println("import com.scalaAsm.x86.Operands._")
    writer.println("")
    writer.println("object " + mnemonic.toUpperCase() + " extends InstructionDefinition[OneOpcode](\"" + mnemonic + "\") with " + mnemonic.toUpperCase() + "Impl")
    writer.println("")
    
    if (!low.isEmpty && !high.isEmpty) {
      writer.println("trait " + mnemonic.toUpperCase() + "Low {")
      for (inst <- low) {
        writer.println(inst.generateClass(2))
        if (inst != low.last)
          writer.println("")
      }
      writer.println("}\n")
      
      writer.println("trait " + mnemonic.toUpperCase() + "Impl extends " + mnemonic.toUpperCase() + "Low {")
      for (inst <- high) {
        writer.println(inst.generateClass(2))
        if (inst != high.last)
          writer.println("")
      }
      writer.println("}")
    } else {
      writer.println("trait " + mnemonic.toUpperCase() + "Impl {")
      for (inst <- instructions) {
        writer.println(inst.generateClass(2))
        if (inst != instructions.last)
          writer.println("")
      }
      writer.println("}")
    }
    writer.close();
  }

  def main(args: Array[String]): Unit = {
    try {

      val insts = loadXML().flatMap{x => x.getInstances}
      outputInstructionFile("ADD", insts.filter(_.mnemonic == "ADD"))
      outputInstructionFile("AND", insts.filter(_.mnemonic == "AND"))
      outputInstructionFile("DEC", insts.filter(_.mnemonic == "DEC"))
      outputInstructionFile("MOV", insts.filter(_.mnemonic == "MOV"))

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