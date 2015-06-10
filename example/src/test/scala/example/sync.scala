package example

/**
 * @author Antares
 */
object sync {
  var exeCount = 0
  
  def getTestID: Int = {
    var returnVal = 0
    this.synchronized {
      exeCount += 1
      println(exeCount)
      returnVal = exeCount
    }
    returnVal
  }
}