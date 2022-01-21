import veritas.Veritas.Test

import scala.language.postfixOps

package test {

  import veritas.PoSharpScript
  import veritas.Veritas.GetOutput

  @Test
  class TestExample {
    def runTest2(): (Boolean, String) = {
      new PoSharpScript("{def a; a = 5; print(a);}")
        .ShouldBe("5")
        .Run()
    }

    def runTest3(): (Boolean, String) = {
      new PoSharpScript("{def a; a = 5; print(a);}")
        .ShouldBe("6")
        .Run()
    }
  }
}
