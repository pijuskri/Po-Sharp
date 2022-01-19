package veritas

import org.reflections.Reflections
import org.reflections.scanners.Scanners.TypesAnnotated
import org.reflections.util.ConfigurationBuilder

import scala.Main.writeToFile
import scala.reflect.internal.util.ScalaClassLoader
import scala.sys.process.Process
import io.AnsiColor._

object Veritas {
  def main(args: Array[String]): Unit = {
    RunTests()
  }

  def GetOutput(input: String): String = {
    val parsed = Parser.parseInput(input);
    val asm = ToAssembly.convertMain(parsed);
    writeToFile(asm, "compiled/", "hello.asm")
    val tmp = Process("wsl make").!!

    tmp.split("\n").last.trim
  }

  /**
   * Runs all tests in the <code>test</code> package. The classes need to be annotated with the <code>@Test</code>
   * annotation. For a method to be considered a test it must be suffixed with "test".
   */
  def RunTests(): Unit = {
    var exitCode = 0
    val out = new StringBuilder

    // reflection stuff
    val reflections = new Reflections(new ConfigurationBuilder()
      .forPackage("test")
      .setScanners(TypesAnnotated))

    // Get all annotated types from package test
    val res = reflections
      .getStore
      .get("TypesAnnotated")
      .get("scala.reflect.ScalaSignature")
      .toArray
      .filter(el => el.asInstanceOf[String].contains("test."))
      .map(el => el.asInstanceOf[String])

    // Get the class and instantiate it
    res.foreach(c => {
      val testClass = ScalaClassLoader(getClass.getClassLoader).tryToInitializeClass(c)
      try {
        val instance = ScalaClassLoader(getClass.getClassLoader).create(c)


        // Run all tests in the class
        testClass.get.getMethods
          .filter(m => m.getName.toLowerCase().contains("test"))
          .foreach(el => {

            val output = el.invoke(instance)
            if (output.asInstanceOf[Boolean]) {
              out.append(s"${el.getName}: $GREEN[PASSED]$RESET")
            } else {
              out.append(s"${el.getName}: $RED[FAILED]$RESET")
              exitCode = 1
            }
          })
      } catch {
        case e: Exception =>
          println(s"Could not instantiate class $c.\n$e")
          System.exit(1)
      }
    })

    println(out)
    System.exit(exitCode)
  }

  class Test extends scala.annotation.ConstantAnnotation {}
}
