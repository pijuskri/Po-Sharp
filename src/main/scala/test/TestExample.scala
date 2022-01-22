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
      new PoSharpScript("{ def a = -1; if(a >= 0) {print(1);} else {print(0);};}")
        .ShouldBe("0")
        .Run()
    }

    def runTest4(): (Boolean, String) = {
      new PoSharpScript("{ def a = 5; while( a >= 0 ) { a = (a - 1); }; print(a);}")
        .ShouldBe("-1")
        .Run()
    }
  }
}
