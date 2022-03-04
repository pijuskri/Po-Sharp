package veritas

import org.reflections.Reflections
import org.reflections.scanners.Scanners.TypesAnnotated
import org.reflections.util.ConfigurationBuilder

import java.io.File
import java.lang.reflect.Method
import java.util.concurrent.{Executors, TimeUnit}
import scala.Main.writeToFile
import scala.io.AnsiColor._
import scala.reflect.internal.util.ScalaClassLoader
import Parser._
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
      Veritas.GetOutput(code, Veritas.getTestName)
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
    val output = Veritas.GetOutput(code, Veritas.getTestName)

    if (expected == output) {
      (true, expected)
    } else {
      (false, s"was $expected")
    }
  }
}

object Veritas {
  private val numOfThreads = 10
  private val chunkSize = 1

  def main(args: Array[String]): Unit = {
    time(() => RunTests())
  }

  /**
   * Times the passed runnable in ms.
   *
   * @param task The task to execute
   */
  def time(task: Runnable): Unit = {
    val now = System.currentTimeMillis

    task.run()

    val timeElapsed = System.currentTimeMillis - now
    println(s"Elapsed time: ${timeElapsed}ms")
  }

  /**
   * Runs all tests in the <code>test</code> package. The classes need to be annotated with the <code>@Test</code>
   * annotation. For a method to be considered a test it must be suffixed with "test".
   */
  def RunTests(): Unit = {
    val pool = Executors.newFixedThreadPool(numOfThreads)

    var exitCode = 0
    val out = new StringBuilder

    // reflection stuff
    val reflections = new Reflections(new ConfigurationBuilder()
      .forPackage("test")
      .setScanners(TypesAnnotated))

    println("Did reflection call")

    // Get all annotated types from package test
    val res = reflections
      .getStore
      .get("TypesAnnotated")
      .get("scala.reflect.ScalaSignature")
      .toArray
      .filter(el => el.asInstanceOf[String].contains("test."))
      .map(el => el.asInstanceOf[String])

    println("Got annotated types")

    // Get the class and instantiate it
    res.foreach(c => {
      val testClass = ScalaClassLoader(getClass.getClassLoader).tryToInitializeClass(c)
      var lastMethodName = ""

      def runTest(instance: AnyRef, el: Method) = {
        // Put output here until all tests are done to avoid using synchronized
        val chunkedOut = new StringBuilder

        // Catches invalid tests (say main is missing from the code snippet)
        try {
          lastMethodName = el.getName
          println(s"About to invoke method $lastMethodName")
          val (output, actual) = el.invoke(instance).asInstanceOf[(Boolean, String)]
          if (output) {
            chunkedOut.append(s"${el.getName}: $GREEN[PASSED]$RESET\n")
          } else {
            chunkedOut.append(s"${el.getName}: $RED[FAILED]$RESET | $actual\n")
            exitCode = 1
          }
        } catch {
          case e: Exception =>
            chunkedOut.append(s"${el.getName}: $RED[ERROR]$RESET Could not instantiate $c.$lastMethodName with: $e\n")
        } finally {
          // Add to actual string builder
          this.synchronized(out.append(chunkedOut.toString))
        }
      }

      try {
        println("Created class loder")
        val instance = ScalaClassLoader(getClass.getClassLoader).create(c)

        // Run all tests in the class
        testClass.get.getMethods.filter(m =>
          m.getName.toLowerCase().contains("test")) // Filter out non-test methods
          .grouped(chunkSize) // Group in chunks
          .foreach(chunk => {
            println("Executing chunk")
            pool.execute(() => {
              chunk.foreach(runTest(instance, _))
            })
          })
      } catch {
        case e: Exception =>
          println(s"$RED[ERROR]$RESET Could not instantiate $c\nException was: $e")
      }
    })

    pool.shutdown()
    pool.awaitTermination(5, TimeUnit.MINUTES)
    println(out)

    // Delete all files created by writeToFile and the tests
    new File("compiled")
      .listFiles
      .filter(_.isFile)
      .filter(_.getName.contains("test"))
      .foreach(el => el.delete())

    System.exit(exitCode)
  }

  def GetOutput(input: String, fileName: String): String = {
    println("In GetOutput")
    val parsed = Parser.parseInput(input)
    println("Parsed Input")
    val asm = ToAssembly.convertMain(parsed)
    println("Converted to ASM")
    writeToFile(asm, "compiled/", s"$fileName.asm")
    println("Written to file")
    val tmp = Process(s"wsl make TARGET_FILE=$fileName").!!
    println("Called process")

    tmp.split("\n").last.trim
  }

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

  class Test extends scala.annotation.ConstantAnnotation {}
}
