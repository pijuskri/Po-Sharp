import veritas.Veritas.Test

import scala.language.postfixOps

package test {

  import veritas.Veritas.GetOutput

  @Test
  class TestExample {
    def runTest(): Boolean = {
      "2" == GetOutput("{def a; a = 5; print(a);}")
    }
  }
}
