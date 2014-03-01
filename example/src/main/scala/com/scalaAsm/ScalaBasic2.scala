package com.scalaAsm

import java.io._
import com.scalaAsm.portableExe.ExeGenerator
import com.scalaAsm.portableExe.OptionalHeader
import scala.util.parsing.combinator.JavaTokenParsers
import com.scalaAsm.asm.AsmCodeSimple
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.x86.x86Registers._
import scala.collection.mutable.HashSet

trait Inst 
case class ADD(x:Inst,y:Inst) extends Inst 
case class MINUS(x:Inst,y:Inst) extends Inst 
case class TIMES(x:Inst,y:Inst) extends Inst 
case class DIVIDE(x:Inst,y:Inst) extends Inst 

case class IMM(x:Int) extends Inst 
case class RESULT(reg: Register32) extends Inst 

class Arith extends JavaTokenParsers { 

  type D = Double 

  def expr:   Parser[Inst]    = term ~ rep(plus | minus)     ^^ {case a~b => (a /: b)((acc,f) => f(acc))} 
  def plus:   Parser[Inst=>Inst] = "+" ~ term                   ^^ {case "+"~b => ADD(_, b)} 
  def minus:  Parser[Inst=>Inst] = "-" ~ term                   ^^ {case "-"~b => MINUS(_, b)} 
  def term:   Parser[Inst]    = factor ~ rep(times | divide) ^^ {case a~b => (a /: b)((acc,f) => f(acc))} 
  def times:  Parser[Inst=>Inst] = "*" ~ factor                 ^^ {case "*"~b => TIMES(_,b) } 
  def divide: Parser[Inst=>Inst] = "/" ~ factor                 ^^ {case "/"~b => DIVIDE(_, b)} 
  def factor: Parser[Inst]    = fpn | "(" ~> expr <~ ")" 
  def fpn:    Parser[Inst]    = floatingPointNumber          ^^ { case x => IMM(x.toInt)} 

} 

object ScalaBasic2 extends Arith {

  def main(args: Array[String]): Unit = {
    try {

        //val input = "(1 + 2 * 3 + 9) * 2 + 1" 
        val input = "1 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2" 
        val result = parseAll(expr, input).get
        
        object HelloWorld3 extends AsmProgram {
          val data = new Data {
		    val pressAnyKey = "Press any key to continue ...\0"
		    val newline = "\r\n\0"
		    val helloWorld = "value: %d\n\0"
		  }
          
          val code = new Code {
		     proc("start") {
		      call("printHelloWorld")
		      push("pressAnyKey")
		      call("flushBuffer")
		      call("waitForKeypress")
		      push("newline")
		      call("flushBuffer")
		      push(imm8(0))
		      call("ExitProcess")
		    }
		
		    proc("printHelloWorld") {
		      
		        trait Computable {
		          def compute: RESULT
		        }
		    	case class PolarAdd(x:Int, y:Int, dst: Register, src: Register) {
		    	  def compute: RESULT = {
		    	    null
		    	  }
		    	}
		        case class PolarMinus(x:Int, y:Int, dst: Register, src: Register) {
		          def compute: RESULT = {
		    	    null
		    	  }
		        }
		        case class PolarTimes(x:Int, y:Int, dst: Register, src: Register) {
		          def compute: RESULT = {
		    	    null
		    	  }
		        }
		        case class PolarDivide(x:Int, y:Int, dst: Register, src: Register) {
		          def compute: RESULT = {
		    	    null
		    	  }
		        }
		        
		        val edi = new EDI
			    val eax = new EAX
			    val ecx = new ECX
			    val ebp = new EBP
			    val edx = new EDX
			    val esp = new ESP 
			  
			    val availableRegs = HashSet(edi, ecx, ebp, edx, eax)

		        def transform(inst: Inst): RESULT = {
		          
		          inst match {
		            case ADD(IMM(x),IMM(y)) => {
		              val regs = availableRegs.take(2).toList
		              val temp = regs(0)
		              val result = regs(1)
		              mov(temp, imm32(x.toInt))
				      add(temp, imm8(y.toInt))
				      mov(result, temp)
				      availableRegs -= result
				      println("returning: " + result)
		              RESULT(result)
		            }
		            case TIMES(IMM(x),IMM(y)) => {
		              availableRegs -= eax
		              val regs = availableRegs.take(3).toList
		              val temp = regs(0)
		              val temp2 = regs(1)
		              val result = regs(2)
		              mov(temp, eax) // save whatever was in eax
		              mov(eax, imm32(x.toInt))
				      mov(temp2, imm32(y.toInt))
				      mul(temp2)
				      mov(result, eax) // restore whatever was in eax
				      mov(eax, temp) // restore whatever was in eax
				      availableRegs += eax
				      RESULT(result)
		            }
		            case ADD(IMM(x),RESULT(y)) => {
		              val regs = availableRegs.take(2).toList
		              val temp = regs(0)
		              val result = regs(1)
		              mov(temp, imm32(x.toInt))
				      add(temp, y)
				      mov(result, temp)
				      availableRegs -= result
				      println("returning: " + result)
		              RESULT(result)
		            }
		            case TIMES(IMM(x),RESULT(y)) => {
		              availableRegs -= eax
		              val regs = availableRegs.take(3).toList
		              val temp = regs(0)
		              val temp2 = regs(1)
		              val result = regs(2)
		              mov(temp, eax) // save whatever was in eax
		              mov(eax, imm32(x.toInt))
				      mov(temp2, y)
				      mul(temp2)
				      mov(result, eax) // restore whatever was in eax
				      mov(eax, temp) // restore whatever was in eax
				      availableRegs += eax
				      RESULT(result)
		            }
		            case ADD(RESULT(x),IMM(y)) => {
		              val regs = availableRegs.take(2).toList
		              val temp = regs(0)
		              val result = regs(1)
		              mov(temp, x)
				      add(temp, imm8(y.toInt))
				      mov(result, temp)
				      availableRegs -= result
				      println("returning: " + result)
		              RESULT(result)
		            }
		            case TIMES(RESULT(x),IMM(y)) => {
		              availableRegs -= eax
		              val regs = availableRegs.take(3).toList
		              val temp = regs(0)
		              val temp2 = regs(1)
		              val result = regs(2)
		              mov(temp, eax) // save whatever was in eax
		              mov(eax, x)
				      mov(temp2, imm32(y.toInt))
				      mul(temp2)
				      mov(result, eax) // restore whatever was in eax
				      mov(eax, temp) // restore whatever was in eax
				      availableRegs += eax
				      RESULT(result)
		            }
		            case ADD(RESULT(x),RESULT(y)) => {
		              add(x, y)
		              RESULT(x)
		            }
		            case ADD(RESULT(x),y) => transform(ADD(RESULT(x), transform(y)))
		            case TIMES(RESULT(x),y) => transform(TIMES(RESULT(x), transform(y)))
		            case ADD(x,IMM(y)) => transform(ADD(transform(x), IMM(y) ))
		            case TIMES(x,IMM(y)) => transform(TIMES(transform(x), IMM(y) ))
		            case ADD(IMM(x),y) => transform(ADD(IMM(x), transform(y) ))
		            case TIMES(IMM(x),y) => transform(TIMES(IMM(x), transform(y) ))
		            case ADD(x,y) => transform(ADD(transform(x), transform(y)))
		            case TIMES(x,y) => transform(TIMES(transform(x), transform(y)))
		            case _ => println(inst); null
		          }
		        }
		        
		        
        println(result)
        val transformed = transform(result)
        builder.codeTokens.foreach(println)
		      
//		      transformed foreach {
//	            case PolarAdd(x, y) =>  mov(eax, imm32(x.toInt)); add(eax, imm8(y.toInt))
//	            case PolarTimes(x, y) => {mov(ecx, imm32(y.toInt)); mul(ecx)}
//	            case _ => 
//	          }
		      
		      
		      
//		      mov(eax, imm32(1.toInt))
//		      add(eax, imm8(3.toInt))
//		      mov(ecx, eax)    
//		      mov(eax, imm32(10.toInt))
//		      mov(edi, imm32(3.toInt))
//		      mul(edi)
//		      add(ecx, eax)
		      
		      
		      push(transformed.reg)
		      push("helloWorld")
		      call("printf")
		      add(esp, imm8(8))
		      retn
		    }
		    
		    builder.codeTokens ++= HelloWorld2.code.builder.codeTokens
          }
        }
         val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
         val assembled = HelloWorld3.assemble
         val exe = ExeGenerator.compile(assembled, 0x2000)
         outputStream.write(exe.get)

         println("done generating")

         outputStream.close
        
          println(parseAll(expr, input).get) // prints 33.0 
          //println(transformed)
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

}