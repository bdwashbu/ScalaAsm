//package com.scalaAsm
//
//import java.io._
//import com.scalaAsm.portableExe.ExeGenerator
//import com.scalaAsm.portableExe.OptionalHeader
//import scala.util.parsing.combinator.JavaTokenParsers
//import com.scalaAsm.asm.AsmCodeSimple
//import com.scalaAsm.asm.AsmProgram
//import com.scalaAsm.x86.Operands._
//import scala.collection.mutable.HashSet
//import com.scalaAsm.asm.Addressing._
//import com.scalaAsm.asm.Tokens.CodeToken
//
//trait Inst 
//case class ADD(x:Inst,y:Inst) extends Inst 
//case class MINUS(x:Inst,y:Inst) extends Inst 
//case class TIMES(x:Inst,y:Inst) extends Inst 
//case class DIVIDE(x:Inst,y:Inst) extends Inst 
//
//case class IMM(x:Int) extends Inst 
//case class RESULT(reg: Register32 with GeneralPurpose) extends Inst 
//
//class Arith extends JavaTokenParsers { 
//
//  def expr:   Parser[Inst]    = term ~ rep(plus | minus)     ^^ {case a~b => (a /: b)((acc,f) => f(acc))} 
//  def plus:   Parser[Inst=>Inst] = "+" ~ term                   ^^ {case "+"~b => ADD(_, b)} 
//  def minus:  Parser[Inst=>Inst] = "-" ~ term                   ^^ {case "-"~b => MINUS(_, b)} 
//  def term:   Parser[Inst]    = factor ~ rep(times | divide) ^^ {case a~b => (a /: b)((acc,f) => f(acc))} 
//  def times:  Parser[Inst=>Inst] = "*" ~ factor                 ^^ {case "*"~b => TIMES(_,b) } 
//  def divide: Parser[Inst=>Inst] = "/" ~ factor                 ^^ {case "/"~b => DIVIDE(_, b)} 
//  def factor: Parser[Inst]    = fpn | "(" ~> expr <~ ")" 
//  def fpn:    Parser[Inst]    = floatingPointNumber          ^^ { case x => IMM(x.toInt)} 
//
//} 
//
//object x86Parser {
//	def parse(expression: String): AsmProgram = {
//	  
//	 new Arith with AsmProgram {
//	  val data = new Data {
//	    val pressAnyKey = "Press any key to continue ...\0"
//	    val newline = "\r\n\0"
//	    val helloWorld = "value: %d\n\0"
//	  }
//	  
//	  val code = new Code {
//	     proc("start") {
//	      call("printHelloWorld")
//	      push("pressAnyKey")
//	      call("flushBuffer")
//	      call("waitForKeypress")
//	      push("newline")
//	      call("flushBuffer")
//	      push(Imm8(0))
//	      call("ExitProcess")
//	    }
//	
//	    proc("printHelloWorld") {
//	      	        
//	    val rdi = new RDI with Addressable[RDI]
//	    val rax = new RAX with Addressable[RAX]
//	    val rcx = new RCX with Addressable[RCX]
//	    val rbp = new RBP with Addressable[RBP]
//	    val rdx = new RDX with Addressable[RDX]
//	    val rsp = new RSP with Addressable[RSP]
//	  
//	    val edi = new EDI with Addressable[EDI]
//	    val eax = new EAX with Addressable[EAX]
//	    val ecx = new ECX with Addressable[ECX]
//	    val ebp = new EBP with Addressable[EBP]
//	    val edx = new EDX with Addressable[EDX]
//	    val esp = new ESP with Addressable[ESP]
//
//	        def transform(inst: Inst, resultReg: Register32 with GeneralPurpose): RESULT = {
//	          
//	          inst match {
//	            case ADD(IMM(x),IMM(y)) => {
//	              mov(resultReg, Imm32(x.toInt))
//			      add(resultReg, Imm8(y.toInt))
//	              RESULT(resultReg)
//	            }
//	            case MINUS(IMM(x),IMM(y)) => {
//	              mov(resultReg, Imm32(x.toInt))
//			      sub(resultReg, Imm8(y.toInt))
//	              RESULT(resultReg)
//	            }
//	            case TIMES(IMM(x),IMM(y)) => {
//		             mov(eax, Imm32(x.toInt))
//				     mov(resultReg, Imm32(y.toInt))
//				     mul(resultReg)  
//				     mov(resultReg, eax)
//				     RESULT(resultReg)
//	            }
//	            case ADD(IMM(x),RESULT(y)) => {
//	              if (resultReg != y) {
//		              mov(resultReg, Imm32(x.toInt))
//				      add(resultReg, y)
//	              } else {
//	                add(resultReg, Imm8(x.toInt))
//	              }
//	              RESULT(resultReg)
//	            }
//	            case ADD(RESULT(x),IMM(z)) => {
//	              transform(ADD(IMM(z), RESULT(x)), resultReg)
//	            }
//	            case MINUS(IMM(x),RESULT(y)) => {
//	              if (resultReg != y) {
//		              mov(resultReg, Imm32(x.toInt))
//				      sub(resultReg, y)
//	              } else {
//	                sub(resultReg, Imm8(x.toInt))
//	              }
//	              RESULT(resultReg)
//	            }
//	            case MINUS(RESULT(x),IMM(z)) => {
//	              if (resultReg != x) {
//		              mov(resultReg, x)
//				      sub(resultReg, Imm8(z.toInt))
//	              } else {
//	                sub(resultReg, Imm8(z.toInt))
//	              }
//	              RESULT(resultReg)
//	            }
//	            case TIMES(IMM(x),RESULT(y)) => {
//	                 mov(eax, Imm32(x.toInt))
//				     mov(resultReg, y)
//				     mul(resultReg)  
//				     mov(resultReg, eax)
//				     RESULT(resultReg)
//	            }
//	            case TIMES(RESULT(y), IMM(x)) => {
//	                 transform(TIMES(IMM(x), RESULT(y)), resultReg)
//	            }
//	            case ADD(RESULT(x),RESULT(z)) => {
//	              mov(resultReg, x)
//			      add(resultReg, z)
//	              RESULT(resultReg)
//	            }
//	            case MINUS(RESULT(x),RESULT(z)) => {
//	              mov(resultReg, x)
//			      sub(resultReg, z)
//	              RESULT(resultReg)
//	            }
//	            case TIMES(RESULT(x),RESULT(y)) => {
//	                 mov(eax, x)
//				     mov(resultReg, y)
//				     mul(resultReg)  
//				     mov(resultReg, eax)
//				     RESULT(resultReg)
//	            }
//	            case ADD(x,IMM(y)) => transform(ADD(transform(x, edi), IMM(y) ), resultReg)
//	            case MINUS(x,IMM(y)) => transform(MINUS(transform(x, edi), IMM(y)), resultReg)
//	            case TIMES(x,IMM(y)) => transform(TIMES(transform(x, edi), IMM(y)), resultReg)
//	            case ADD(IMM(x),y) => transform(ADD(IMM(x), transform(y, edx)), resultReg)
//	            case MINUS(IMM(x),y) => transform(MINUS(IMM(x), transform(y, edx)), resultReg)
//	            case TIMES(IMM(x),y) => transform(TIMES(IMM(x), transform(y, edx)), resultReg)
//	            case ADD(x,y) => transform(ADD(transform(x, edi), transform(y, edx)), resultReg)
//	            case MINUS(x,y) => transform(MINUS(transform(x, edi), transform(y, edx)), resultReg)
//	            case TIMES(x,y) => transform(TIMES(transform(x, edi), transform(y, edx)), resultReg)
//	            case _ => if (inst != null) println(inst); null
//	          }
//	        }	        
//	        val result = parseAll(expr, expression).get
//	        val transformed = transform(result, ebp)
//	        builder.importantTokens ++= builder.codeTokens.drop(10)
//	        //builder.codeTokens.foreach(println)
//	
//	      push(transformed.reg)
//	      push("helloWorld")
//	      call("printf")
//	      add(esp, Imm8(8))
//	      retn
//	    }
//	    
//	    
//	    builder.codeTokens ++= HelloWorld2.code.builder.codeTokens
//	  } 
//	}
//	 
//	}
//	
//	def getCodeString(app: AsmProgram): List[String] = {
//	  val codeTokens = app.code.builder.importantTokens.collect{ case(CodeToken(x)) => x}.map(_.toString)
//	  codeTokens.toList
//	}
//}
//
//object ScalaBasic2 extends Arith {
//
//  def parseExpr(expr: String) = {
//    
//  }
//  
//  def main(args: Array[String]): Unit = {
//    try {
//
//        //val input = "2*2*2*2" 
//        val input = "3*(1 + 2)"
//        
//        
//        
//         val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
//         val assembled = x86Parser.parse(input).assemble
//         val exe = ExeGenerator.compile(assembled, 0x2000)
//         outputStream.write(exe.get)
//
//         println("done generating")
//
//         outputStream.close
//        
//          println(parseAll(expr, input).get) // prints 33.0 
//          //println(transformed)
//    } catch {
//      case e: Exception => e.printStackTrace()
//    }
//  }
//
//}
//
//object HelloWorld2 extends AsmProgram {
//
//  val data = new Data {}
//
//  val code = new Code {
//
//    proc("flushBuffer") {
//
//      val numberOfBytesToWrite = *(ebp - byte(12))
//      val numberOfBytesWritten = *(ebp + byte(-8))
//      val hFile = *(ebp + byte(-4))
//      val lpBuffer = *(ebp + byte(8))
//      val STD_OUTPUT_HANDLE = Imm8(-11)
//
//      push(ebp)
//      mov(ebp, esp)
//      add(esp, Imm8(-12))
//      push(STD_OUTPUT_HANDLE)
//      call("GetStdHandle")
//      mov(hFile, eax)
//      push(lpBuffer)
//      call("strlen")
//      mov(numberOfBytesToWrite, eax)
//      push(Imm8(0))
//      lea(eax, numberOfBytesWritten)
//      push(eax)
//      push(numberOfBytesToWrite)
//      push(lpBuffer)
//      push(hFile)
//      call("WriteFile")
//      mov(eax, numberOfBytesWritten)
//      leave
//      retn(Imm16(4))
//    }
//
//    align(0x10)
//
//    proc("waitForKeypress") {
//
//      val STD_INPUT_HANDLE = Imm8(-10)
//
//      push(STD_INPUT_HANDLE)
//      call("GetStdHandle")
//      push(eax)
//      call("FlushConsoleInputBuffer")
//      push(Imm8(1))
//      call("Sleep")
//      call("_kbhit")
//      test(eax, eax)
//      jz(Imm8(-17))
//      call("_getch")
//      retn
//    }
//
//    align(0x10)
//
//    proc("strlen") {
//
//      mov(eax, *(esp + byte(4))) // pointer to string
//      lea(edx, *(eax + byte(3)))
//      push(ebp)
//      push(edi)
//      mov(ebp, Imm32(0x80808080))
//
//      label("start")
//
//      for (i <- 0 until 3) {
//        mov(edi, *(eax)) // read first 4 bytes
//        add(eax, Imm8(4)) // increment pointer
//        lea(ecx, *(edi - dword(0x1010101))) // subtract 1 from each byte
//        not(edi) // invert all bytes
//        and(ecx, edi)
//        and(ecx, ebp)
//        jnz("test")
//      }
//
//      mov(edi, *(eax))
//      add(eax, Imm8(4))
//      lea(ecx, *(edi - dword(0x1010101)))
//      not(edi)
//      and(ecx, edi)
//      and(ecx, ebp)
//      jz("start")
//
//      label("test")
//      test(ecx, Imm32(0x8080)) // test first 2 bytes
//      jnz("end")
//      shr(ecx, Imm8(0x10))
//      add(eax, Imm8(2))
//      label("end")
//      shl(cl, One)
//      sbb(eax, edx) // compute length
//      pop(edi)
//      pop(ebp)
//      retn(Imm16(4))
//    }
//
//    align(2)
//
//    proc("ExitProcess") {
//      jmp("ExitProcess")
//    }
//
//    proc("GetStdHandle") {
//      jmp("GetStdHandle")
//    }
//
//    proc("WriteFile") {
//      jmp("WriteFile")
//    }
//
//    proc("FlushConsoleInputBuffer") {
//      jmp("FlushConsoleInputBuffer")
//    }
//
//    proc("Sleep") {
//      jmp("Sleep")
//    }
//  }
//}