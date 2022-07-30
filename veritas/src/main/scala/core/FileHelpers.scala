package core

import posharp.Main.writeToFile

import java.io.File
import scala.sys.process._

object FileHelpers {

  /**
   * Writes the code to a file, executes it and returns the output.
   *
   * @param asm      Assembly
   * @param fileName The filename
   * @return The last thing printed by the code
   */
  def GetOutput(asm: String, fileName: String): String = {
    writeToFile(asm, "../compiled/", s"$fileName.ll")

    val prefix = if (IsWindows()) {
      "wsl "
    } else {
      ""
    }

    val cmd = s"$prefix make build -B TARGET_FILE=$fileName"

    val tmp = Process.apply(cmd, Option(new File("../"))).!!

    tmp.split("\n").last.trim
  }

  private def IsWindows(): Boolean = System.getProperty("os.name").toLowerCase().contains("windows")

  /**
   * Creates a unique-enough™ filename for the current test by concatenating the class name the test comes from with
   * the test name itself.
   *
   * @return The test name
   */
  def getTestName: String = {
    val stackTrace = new Throwable().getStackTrace()(2)
    val className = stackTrace.getClassName
    val methodName = stackTrace.getMethodName

    s"$className.$methodName"
  }

  /**
   * Deletes all files created by writeToFile and the tests.
   */
  def deleteTestArtifacts(): Unit = {
    new File("../compiled")
      .listFiles
      .filter(_.isFile)
      .filter(_.getName.contains("test"))
      .foreach(el => el.delete())
  }
}
