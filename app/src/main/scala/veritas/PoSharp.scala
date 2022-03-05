package veritas

import scala.language.implicitConversions

protected trait IPoSharp {
  /**
   * Asserts that the expected value is equal to the output of the code snippet.
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