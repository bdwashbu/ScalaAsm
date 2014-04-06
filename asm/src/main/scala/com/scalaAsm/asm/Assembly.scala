package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer
import com.scalaAsm.asm.Tokens.Token

case class Assembled(val code: Seq[Token], val data: Seq[Token])

trait AsmProgram {
  
  val codeSegments = new ListBuffer[CodeSection]()
  val dataSegments = new ListBuffer[DataSection]()

  def assemble: Assembled = {
    val codeTokens: ListBuffer[Token] = codeSegments.flatMap{seg => seg.build(seg.builder.toList)}
    
    val dataTokens = dataSegments.flatMap{seg => seg.compile}

    Assembled(codeTokens, dataTokens)
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