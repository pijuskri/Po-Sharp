package core

import core.FileHelpers.deleteTestArtifacts

import posharp.{Expr, Parser, ToAssembly}
import test.TestExample

import java.lang.reflect.Method
import java.util.concurrent.{Executors, TimeUnit}
import scala.collection.mutable
import scala.io.AnsiColor.*
import scala.util.{Failure, Success, Try}

// TODO: Un-hardcode classes and use annotations
val CLASSES_TO_TEST = List(TestExample().getClass.getName);

object Veritas {
  private val numOfThreads = 10
  private val chunkSize = 1
  private var cov: Coverage.type = _
  private var calculateCoverage = false
  private var exportCoverage = false

  /**
   * Runs all tests. If the first argument is `coverage`, coverage is calculated and printed.
   *
   * Command line arguments:
   * <ul>
   * <li>[0] - `coverage`: coverage is calculated and printed</li>
   * <li>[1] - `export`: coverage is exported in CodeCov JSON format</li>
   * </ul>
   *
   * Order matters!
   *
   * @param args Command line arguments.
   */
  def main(args: Array[String]): Unit = {
    println(args.mkString("args=Array(", ", ", ")"))
    if (args.isDefinedAt(0) && args.head == "coverage") {
      calculateCoverage = true
      cov = Coverage

      if (args.isDefinedAt(1) && args(1) == "export")
        exportCoverage = true
    }

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
    val out = new mutable.StringBuilder
    out.append('\n')

    println()

    // Get the class and instantiate it
    CLASSES_TO_TEST.foreach(c => {
      val testClass = Class.forName(c).getConstructor().newInstance().getClass

      var lastMethodName = ""

      def runTest(instance: AnyRef, tests: Array[Method]): Unit = {
        // Put output here until all tests are done to avoid using synchronized
        val chunkedOut = new mutable.StringBuilder

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
        val instance = Class.forName(c).getConstructor().newInstance().asInstanceOf[AnyRef]

        // Run all tests in the class
        testClass.getMethods.filter(m => m.getName.toLowerCase().contains("test")) // Filter out non-test methods
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

    if (calculateCoverage)
      cov.CalculateCoverage(exportCoverage)

    deleteTestArtifacts()

    exitCode
  }

  /**
   * Parses and compiles the code to asm
   *
   * @param input The code
   * @return Generated assembly
   * @note [[ToAssembly.convertMain]] alters `ToAssembly`'s state and thus needs to be synchronized.
   */
  def Compile(input: String): Try[String] = {
    try {
      val parsed = Parser.parseInput(input, "file_name").get
      val something = new ToAssembly("file_name")
      something.declarationPass(parsed)

      if (calculateCoverage)
        cov.AddCoverage(parsed)

      this.synchronized(Success(something.convertMain(parsed, Map[String, Expr.TopLevel]())))
    } catch {
      case e: Exception => Failure(e)
    }
  }

  class Test extends scala.annotation.ConstantAnnotation {}

  case class InvalidReturnTypeException(msg: String) extends RuntimeException(msg)
}
