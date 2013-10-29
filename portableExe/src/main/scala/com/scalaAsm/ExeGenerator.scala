package com.scalaAsm.portableExe

import java.io.{ OutputStream }
import java.nio.{ ByteBuffer, ByteOrder }
import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeMap
import java.io.DataOutputStream
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import com.scalaAsm.asm.Assembled
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.utils.Endian
  
 case class CompiledImports(val rawData: Array[Byte],
                            val boundImportSize: Int,
                            val nameTableSize: Int,
                            val imports: Map[String, Int],
                            val externs: Map[String, Int]) {
  
  def getImportsDirectory(optionalHeader: OptionalHeader, dataSize: Int) = {
    
    val importsLocation = optionalHeader.addressOfData + dataSize
    Directory(importsLocation, boundImportSize)
  }
  
  def getIATDirectory(optionalHeader: OptionalHeader, dataSize: Int) = {
    
    val endOfData = optionalHeader.addressOfData + dataSize
    val IATlocation = endOfData + boundImportSize + nameTableSize
    Directory(IATlocation, nameTableSize)
  }  
}

 case class CompiledData(val rawData: Array[Byte],
                         val variables: Map[String, Int])


object ExeGenerator extends Sections {
  
  def align(array: Array[Byte], to: Int, filler: Byte = 0xCC.toByte) = {
    val currentSize = array.size
    val numPadding = (to - (currentSize % to)) % to
    array ++ Array.fill(numPadding)(filler)
  }

  def compileData(addressOfData: Int, dataTokens: Seq[Token]): CompiledData = {

    val dataSection: Seq[PostToken] = {
      var parserPosition = 0
      for (token <- dataTokens) yield {
        val result = token match {
	      case Variable(name, value, _) => PostVar(name, value, parserPosition)
	      case Align(to, filler, _) => ByteOutputPost( Array.fill((to - (parserPosition % to)) % to)(filler))
        }
        token match {
          case sizedToken: SizedToken => parserPosition += sizedToken.size
          case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
        }
        
        result
      }      
    }
    
    val dataBytes = (dataSection.flatMap { token =>
      token match {
        case ByteOutputPost(padding) => Some(padding)
        case PostVar(_,value,_) => Some(value.toCharArray().map(_.toByte))
        case _ => None
      }})
      
    val data = Array.fill[Byte](8)(0x00) ++ dataBytes.reduce(_++_)

    // a map of variable to its RVA
    def createDefMap: Map[String, Int] = {
    	dataSection.flatMap {
	      case PostVar(name, value, pos) => Some((name, pos + addressOfData + 8))
	      case _ => None
	    }.toMap
    }
    
    CompiledData(data, createDefMap)
  }
  
  def compileImports(addressOfData: Int, dataSize: Int): CompiledImports = { 
    
    val test = Imports(externs = Seq(Extern("kernel32.dll", List("ExitProcess", "GetStdHandle", "WriteFile", "FlushConsoleInputBuffer", "Sleep"))),
                       nonExterns = Seq(Extern("msvcrt.dll", List("printf", "_kbhit", "_getch"))),
                       offset = addressOfData + dataSize)
    
    test.link
  }
    
  case class ExeParts(val code: Array[Byte], val compiledData: CompiledData, val compiledImports: CompiledImports)
  
  private def buildExe(optionalHeader: OptionalHeader, asm: Assembled): ExeParts = {

    lazy val compiledData = compileData(optionalHeader.addressOfData, asm.data)
    lazy val compiledImports = compileImports(optionalHeader.addressOfData, compiledData.rawData.size)

    lazy val importNames = compiledImports.imports.keys.toList
    lazy val externNames = compiledImports.externs.keys.toList
    lazy val varNames = compiledData.variables.keys.toList
    lazy val procNames = asm.code.collect{ case BeginProc(name) => name }
    
    def onePass: Seq[Token] = (asm.code.flatMap { (token) =>
      token match {
        case x: SizedToken => Some(x)
        case x: DynamicSizedToken => Some(x)
        case JmpRef(name) if externNames.contains(name) => Some(JmpRefResolved(name))
        case Reference(name) if importNames.contains(name) => Some(ImportRef(name))
        case Reference(name) if procNames.contains(name) => Some(ProcRef(name))
        case Reference(name) if varNames.contains(name) => Some(VarRef(name))
        case Reference(_) => throw new Exception("no reference found!")
        case Label(name) => Some(Label(name))
        case LabelRef(name,opcode) => Some(LabelRef(name,opcode))
        case _ => None
      }
    })

    def twoPass: Seq[PostToken] = {
      var parserPosition = 0
      for (token <- onePass) yield {
        val result = token match {
	        case CodeToken(code) => ByteOutputPost(code)
	        case Align(to, filler, _) => ByteOutputPost(Array.fill((to - (parserPosition % to)) % to)(filler))
	        case Padding(to, _) => ByteOutputPost(Array.fill(to)(0xCC.toByte))
	        case BeginProc(name) => Proc(parserPosition, name)
	        case ProcRef(name) => ProcState(parserPosition, name)
	        case VarRef(name) => VarState(parserPosition, name)
	        case JmpRefResolved(name) => JmpState(parserPosition, name)
	        case ImportRef(name) => ImportState(parserPosition, name)
	        case Label(name) => LabelResolved(parserPosition, name)
	        case LabelRef(name, opCode) => LabelRefResolved(parserPosition, name, opCode)
	      }
         token match {
          case sizedToken: SizedToken => parserPosition += sizedToken.size
          case sizedToken: DynamicSizedToken => parserPosition += sizedToken.size(parserPosition)
          case x:LabelRef => parserPosition += 2
          case _ => 
         }
         result
      }
    }
    
    // Build procedure map
    val procs = twoPass.collect{case Proc(offset, name) => (name, offset)}.toMap
    val labels = twoPass.collect{case LabelResolved(offset, name) => (name, offset)}.toMap

    val code: Array[Byte] = twoPass.map { token =>
      token match {
        case ByteOutputPost(code) => code
        case ProcState(offset, name) => 0xE8.toByte +: Endian.swap(procs(name) - offset - 5)
        case VarState(_, name) => 0x68.toByte +: Endian.swap(compiledData.variables(name) + optionalHeader.imageBase)
        case ImportState(_, name) => Array[Byte](0xFF.toByte, 0x15) ++ Endian.swap(compiledImports.imports(name) + optionalHeader.imageBase)
        case JmpState(_, name) => Array[Byte](0xFF.toByte, 0x25) ++ Endian.swap(compiledImports.externs(name) + optionalHeader.imageBase)
        case LabelRefResolved(offset, name, opCode) => Array(opCode, (labels(name) - offset - 2).toByte)
        case _ => Array[Byte]()
      }
    }.reduce(_ ++ _)
    
    ExeParts(code, compiledData, compiledImports) 
  }
  
  def compile(asm: Assembled): PortableExecutable = {

    val optionalHeader = new OptionalHeader
    val ExeParts(code, data, imports) = buildExe(optionalHeader, asm)
    
    val directories = DataDirectories(imports = imports.getImportsDirectory(optionalHeader, data.rawData.size),
                                      importAddressTable = imports.getIATDirectory(optionalHeader, data.rawData.size))
    
    val sections = compileSections(code.size, data.rawData.size + imports.rawData.size)
    
    val dosHeader = new DosHeader
    val peHeader = new PeHeader(optionalHeader, directories)
    
    new PortableExecutable(dosHeader, peHeader, optionalHeader, directories, sections, code, data, imports)
  }
  
  
}