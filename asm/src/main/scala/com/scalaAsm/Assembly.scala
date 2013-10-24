package com.scalaAsm.asm

import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.collection.mutable.ListBuffer
import com.scalaAsm.utils.Endian
import com.scalaAsm.asm.Tokens._

case class Assembled(val code: Seq[Token], val data: Seq[Token]) {
  def rawCode: Array[Byte] = code.collect{ case CodeToken(x) => x}.reduce(_++_)
}

trait CodeBuilder {
  val codeTokens = ListBuffer[Any]()
}

abstract class AsmProgram extends AsmData with AsmCode {

  def hex2Bytes(hex: String) = {
    def stripChars(s: String, ch: String) = s filterNot (ch contains _)
    stripChars(hex, " -").grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
  }

  def alignStream(stream: Array[Byte], x: Int) = {
    val size = stream.size
    val poo = Array.fill[Byte]((x - (size % x)) % x)(0x00.toByte)
    stream ++ poo
  }

  def assemble = {
    val tokens = ListBuffer[Any]()
    for (token <- code.builder.codeTokens) {
      if (token.isInstanceOf[Proc]) {
        val proc = token.asInstanceOf[Proc]
        tokens ++= (BeginProc(proc.name) +: proc.builder.codeTokens :+ EndProc(proc.name))
      }
      else
      {
        tokens += token.asInstanceOf[Token]
      }
    }

    Assembled(tokens.asInstanceOf[ListBuffer[Token]].toSeq, dataTokens)
  }

//  def getAssembledPermutations: Iterator[Assembled] = {
//    val procs = execution.toList
//    procs.drop(1).toStream.combinations(4).map { perm =>
//      val head = procs.head
//      val tokens: List[Code] = List(head) ++ perm.map(x => x.codeTokens).reduce(_++_) ++ procs.filter { x => !perm.contains(x) }.map(x => x.codeTokens)
//      // "start" function must be at the start
//      val result = tokens.map(_.codeTokens).reduce(_ ++ _)
//      Assembled(result.toSeq, dataTokens.toSeq)
//    }
//  }
}