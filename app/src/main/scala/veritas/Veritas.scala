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
import scala.sys.process.Process
import scala.util.{Failure, Success, Try}

object Veritas {
  private val numOfThreads = 10
  private val chunkSize = 1
  private val cov = Coverage

  def main(args: Array[String]): Unit = {
    var exitCode = 0

    time(() => exitCode = RunTests())

    System.exit(exitCode)
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
  def RunTests(): Int = {
    val pool = Executors.newFixedThreadPool(numOfThreads)

    var exitCode = 0
    val out = new StringBuilder
    out.append('\n')

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
      .filter(_.asInstanceOf[String].contains("test."))
      .map(_.asInstanceOf[String])

    println()

    // Get the class and instantiate it
    res.foreach(c => {
      val testClass = ScalaClassLoader(getClass.getClassLoader).tryToInitializeClass(c)
      var lastMethodName = ""

      def runTest(instance: AnyRef, tests: Array[Method]): Unit = {
        // Put output here until all tests are done to avoid using synchronized
        val chunkedOut = new StringBuilder

        // Catches invalid tests (say main is missing from the code snippet)
        tests.foreach(el => {
          // Catches invalid tests (say main is missing from the code snippet)
          try {
            lastMethodName = el.getName

            val (output, actual) = el.invoke(instance) match {
              case (output: Boolean, actual: String) => (output, actual)
              case _ => throw InvalidReturnTypeException("Invalid test method return type. Should be (Boolean, String)")
            }

            if (output) {
              chunkedOut.append(s"$GREEN[PASSED]$RESET: ${el.getName}\n")
            } else {
              chunkedOut.append(s"$RED[FAILED]$RESET: ${el.getName} | $actual\n")
              exitCode = 1
            }
          } catch {
            case _: Exception =>
              chunkedOut.append(s"$RED[ERROR]$RESET : ${el.getName} Could not instantiate $c.${el.getName}" +
                s", check logs above for more info.\n")
              exitCode = 1
          }
        })

        // Add to actual string builder
        this.synchronized(out.append(chunkedOut.toString))
      }

      try {
        val instance = ScalaClassLoader(getClass.getClassLoader).create(c)

        // Run all tests in the class
        testClass.get.getMethods.filter(m =>
          m.getName.toLowerCase().contains("test")) // Filter out non-test methods
          .grouped(chunkSize) // Group in chunks
          .foreach(chunk => {
            pool.execute(() => runTest(instance, chunk))
          })
      } catch {
        case e: Exception =>
          println(s"$RED[ERROR]$RESET Could not instantiate $c\nException was: $e")
      }
    })

    pool.shutdown()
    pool.awaitTermination(5, TimeUnit.MINUTES)
    println(out)
    println()
    cov.CalculateCoverage().foreach(println)

    // Delete all files created by writeToFile and the tests
    new File("compiled")
      .listFiles
      .filter(_.isFile)
      .filter(_.getName.contains("test"))
      .foreach(el => el.delete())

    exitCode
  }

  /**
   * Parses and compiles the code to asm
   *
   * @param input The code
   * @return Generated assembly
   * @note [[ToAssembly.convertMain]] alters `ToAssembly`'s state and thus needs to be synchronized.
   */
  def Compile(input: String): Try[String] = this.synchronized {
    try {
      val parsed = Parser.parseInput(input)

      cov.AddCoverage(parsed)

      Success(ToAssembly.convertMain(parsed))
    } catch {
      case e: Exception => Failure(e)
    }
  }

  /**
   * Writes the code to a file, executes it and returns the output.
   *
   * @param asm      Assembly
   * @param fileName The filename
   * @return The last thing printed by the code
   */
  def GetOutput(asm: String, fileName: String): String = {
    writeToFile(asm, "compiled/", s"$fileName.asm")

    val tmp = Process(if (IsWindows()) {
      "wsl "
    } + s"make TARGET_FILE=$fileName" else {
      ""
    }).!!

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

  class Test extends scala.annotation.ConstantAnnotation {}

  case class InvalidReturnTypeException(msg: String) extends RuntimeException(msg)
}
