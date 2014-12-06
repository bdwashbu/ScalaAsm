package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.libs.iteratee.Enumerator
import play.api.data.Forms._
import com.scalaAsm.portableExe._
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.File
import com.scalaAsm.asm.AsmProgram
import play.api.libs.concurrent.Execution.Implicits._
import play.api.data.validation.Constraints._
import com.play_interface.x86Parser

import views._

case class x86App(expression: String)

object Application extends Controller {

  /**
   * Describes the hello form.
   */

  def blah(expr: String): Boolean = {
    try {
      val app = x86Parser.parse(expr)
      true
    } catch {
      case _: Throwable => false
    }
  }

  val helloForm = Form(
    mapping(
      "expression" -> (
        nonEmptyText(maxLength=20)
        verifying ("Invalid math expression!", expr =>
          blah(expr))))(x86App.apply)(x86App.unapply))

  // -- Actions

  /**
   * Home page
   */

//    def index = Action {
//      Ok("HEllo!")
//    }
  //  
  //  def sayHello = Action { implicit request =>
  //    Ok("GET FILE!")
  //  }

    def index = Action {
      Ok(html.index(helloForm))
    }
    
  def getFile(instructions: String) = Action {
    val assembler = new Assembler {}
    val linker = new Linker {}

    val app = x86Parser.parse(instructions)
    val assembled = assembler.assemble(app)
    val exe = linker.link(assembled, 0x3000, false, "kernel32.dll", "msvcrt.dll")

    val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
    outputStream.write(exe.get)
    val fileContent: Enumerator[Array[Byte]] = Enumerator.fromFile(new File("test.exe"))

    Ok.sendFile(
      content = new java.io.File("test.exe"),
      fileName = _ => "result.exe")
  }

//  helloForm.bindFromRequest.fold(
//    formWithErrors => BadRequest(html.index(formWithErrors)),
//    {
//      case (expression) => {
//        val assembler = new Assembler {}
//        val linker = new Linker {}
//     
//        val app = x86Parser.parse(expression.expression)
//        
//        val assembled = assembler.assemble(app)
//        val exe = linker.link(assembled, 0x3000, false, "kernel32.dll", "msvcrt.dll")
//
//        val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
//        outputStream.write(exe.get)
//        val fileContent: Enumerator[Array[Byte]] = Enumerator.fromFile(new File("test.exe"))
//
//        Ok.sendFile(
//          content = new java.io.File("test.exe"),
//          fileName = _ => "result.exe")
//      }
//    })

  /**
   * Handles the form submission.
   */
  def sayHello = Action { implicit request =>
    helloForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.index(formWithErrors)),
      {
        case (expression) => {
          val assembler = new Assembler {}
          val linker = new Linker {}
        
          val app = x86Parser.parse(expression.expression)
          
          val assembled = assembler.assemble(app)
          val exe = linker.link(assembled, 0x3000, false, "kernel32.dll", "msvcrt.dll")

          Ok(html.hello((x86Parser.getCodeString(app), expression.expression)))
        }
      })
  }

}
