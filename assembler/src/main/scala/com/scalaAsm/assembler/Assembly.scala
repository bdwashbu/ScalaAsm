package com.scalaAsm.assembler

import scala.collection.mutable.ListBuffer
import com.scalaAsm.asm.Tokens.Token
import com.scalaAsm.linker.Assembled
import com.scalaAsm.asm.CodeSection
import com.scalaAsm.asm.DataSection

trait AsmProgram {
  
  val codeSections = new ListBuffer[CodeSection]()
  val dataSections = new ListBuffer[DataSection]()

  def assemble: Assembled = {
    val codeTokens: ListBuffer[Any] = codeSections.flatMap{seg => seg.build(seg.builder.toSeq)}
    
    val dataTokens = dataSections flatMap {seg => seg.compile}

    new AsmCompiler(codeTokens, dataTokens)
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