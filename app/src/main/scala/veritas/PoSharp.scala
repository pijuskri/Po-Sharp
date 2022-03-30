package veritas

import scala.language.implicitConversions
import scala.util.{Success, Failure}

protected trait IPoSharp {
  /**
   * Sets the expected value for the test.
   *
   * @param expected The expected value.
   * @return True iff the expected value matches the output of the code snippet.
   */
  def ShouldBe(expected: String): PoSharp.PoSharpScript

  /**
   * Asserts that the given exception is thrown when the snippet executes.
   *
   * @note This is a terminal method, i.e. you do not need to call Run after it.
   * @param expected The expected exception.
   * @return True iff the exception thrown has the same type as the expected one.
   */
  def ShouldThrow(expected: Throwable): (Boolean, String)
}

object PoSharp {
  /**
   * Holds a code snippet to execute for testing purposes.
   *
   * @param code The code snippet.
   */
  case class PoSharpScript(code: String) extends IPoSharp {
    private var expected = ""

    def ShouldBe(expected: String): PoSharpScript = {
      this.expected = expected
      this
    }

    def ShouldThrow(expected: Throwable): (Boolean, String) = {
      Veritas.Compile(code) match {
        case Failure(exception) => exception match {
          case e: Exception if e.getClass == expected.getClass => (true, e.getMessage)
          case e: Exception => (false, s"Expected \"$expected.type\", \"$e.type\" was thrown instead")
        }
        case Success(_) => (false, "No exception was thrown")
      }
    }

    /**
     * Compile, run the code snippet and check assertions.
     *
     * @return (Passed/Failed, Failure debug message)
     */
    def Run(): (Boolean, String) = {
      Veritas.Compile(code) match {
        case Success(value) =>
          val output = Veritas.GetOutput(value, Veritas.getTestName)

          if (expected == output) {
            (true, expected)
          } else {
            (false, s"Expected $expected, was $output")
          }
        case Failure(exception) =>
          println(s"Compilation failed with $exception")
          throw  exception
      }

    }
  }

  /**
   * Implicit wrapper for PoSharpScript
   *
   * @param code The code snippet
   */
  implicit class PoSharpImplicit(val code: String) extends IPoSharp {
    def ShouldBe(expected: String): PoSharpScript = PoSharpScript(code).ShouldBe(expected)

    def ShouldThrow(expected: Throwable): (Boolean, String) = PoSharpScript(code).ShouldThrow(expected)
  }
}