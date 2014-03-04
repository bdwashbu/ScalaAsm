package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.libs.iteratee.Enumerator
import play.api.data.Forms._
import com.scalaAsm.x86Parser
import com.scalaAsm.portableExe.ExeGenerator
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.File
import com.scalaAsm.asm.AsmProgram
import play.api.libs.concurrent.Execution.Implicits._
import play.api.data.validation.Constraints._

import views._

case class x86App(expression: String)

object Application extends Controller {

  /**
   * Describes the hello form.
   */

  
  val helloForm = Form(
	  mapping(
	    "expression" -> (
	        nonEmptyText 
	        verifying ("Invalid math expression", expr => { 
	          try {
	            val app = x86Parser.parse(expr)
	            true
	          } catch {
	            case _ => false
	          }
	       }))
	  )(x86App.apply)(x86App.unapply)
  )

  // -- Actions

  /**
   * Home page
   */
  def index = Action {
    Ok(html.index(helloForm))
  }

  
  /**
   * Handles the form submission.
   */
  def sayHello = Action { implicit request =>
    helloForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.index(formWithErrors)),
      {case (expression) => {
        val app = x86Parser.parse(expression.expression)
        val assembled = app.assemble
        val exe = ExeGenerator.compile(assembled, 0x2000)
        
        val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
        outputStream.write(exe.get)
        val fileContent: Enumerator[Array[Byte]] = Enumerator.fromFile(new File("test.exe"))
        
        SimpleResult(
		    header = ResponseHeader(200),
		    body = fileContent
		  )
		  
		 Ok.sendFile(
		    content = new java.io.File("test.exe"),
		    fileName = _ => "result.exe"
		  )
      }
      }
    )
  }

}
