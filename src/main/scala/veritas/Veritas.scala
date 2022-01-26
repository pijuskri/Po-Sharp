package veritas

import org.reflections.Reflections
import org.reflections.scanners.Scanners.TypesAnnotated
import org.reflections.util.ConfigurationBuilder

import scala.Main.writeToFile
import scala.io.AnsiColor._
import scala.reflect.internal.util.ScalaClassLoader
import scala.sys.process.Process

/**
 * Holds a code snippet to execute for testing purposes.
 *
 * @param code The code snippet.
 */
class PoSharpScript(code: String) {
  private var expected = ""

  /**
   * Asserts that the expected value is equal to the output of the code snippet.
   *
   * @param expected The expected value.
   * @return True iff the expected value matches the output of the code snippet.
   */
  def ShouldBe(expected: String): PoSharpScript = {
    this.expected = expected
    this
  }

  /**
   * Asserts that the given exception is thrown when the snippet executes.
   *
   * @note This is a terminal method, i.e. you do not need to call Run after it.
   * @param expected The expected exception.
   * @return True iff the exception thrown has the same type as the expected one.
   */
  def ShouldThrow(expected: Throwable): (Boolean, String) = {
    try {
      Veritas.GetOutput(code)
    } catch {
      case e: Exception => return handleException(e)
    }

    def handleException(e: Exception): (Boolean, String) = {
      if (expected.getClass == e.getClass)
        (true, e.getMessage)
      else
        (false, s"Expected \"$expected.type\", \"$e.type\" was thrown instead")
    }

    (false, "No exception was thrown")
  }

  /**
   * Compile, run the code snippet and check assertions.
   *
   * @return
   */
  def Run(): (Boolean, String) = {
    val output = Veritas.GetOutput(code)

    if (expected == output) {
      (true, expected)
    } else {
      (false, s"was $expected")
    }
  }
}

object Veritas {
  def main(args: Array[String]): Unit = {
    RunTests()
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
      var lastMethodName = ""
      try {
        val instance = ScalaClassLoader(getClass.getClassLoader).create(c)


        // Run all tests in the class
        testClass.get.getMethods
          .filter(m => m.getName.toLowerCase().contains("test"))
          .foreach(el => {
            // Catches invalid tests (say main is missing from the code snippet)
            try {
              lastMethodName = el.getName
              val (output, actual) = el.invoke(instance).asInstanceOf[(Boolean, String)]
              if (output) {
                out.append(s"${el.getName}: $GREEN[PASSED]$RESET\n")
              } else {
                out.append(s"${el.getName}: $RED[FAILED]$RESET | $actual\n")
                exitCode = 1
              }
            } catch {
              case e: Exception =>
                println(s"$RED[ERROR]$RESET Could not instantiate $c.$lastMethodName with: $e")
            }
          })
      } catch {
        case e: Exception =>
          println(s"$RED[ERROR]$RESET Could not instantiate $c\nException was: $e")
          System.exit(1)
      }
    })

    println(out)
    System.exit(exitCode)
  }

  def GetOutput(input: String): String = {
    val parsed = Parser.parseInput(input);
    val asm = ToAssembly.convertMain(parsed);
    writeToFile(asm, "compiled/", "hello.asm")
    val tmp = Process("wsl make").!!

    tmp.split("\n").last.trim
  }

  class Test extends scala.annotation.ConstantAnnotation {}
}
