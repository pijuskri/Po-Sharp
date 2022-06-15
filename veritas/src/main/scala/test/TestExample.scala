package test

import core.PoSharp.PoSharpImplicit
import core.Veritas.Test
import posharp.Parser.ParseException

import scala.language.postfixOps


@Test
class TestExample {
  def runTest2(): (Boolean, String) =
    """def main(): int {
         val a = 5;
         print(a);
         return 0;
       }"""
      .ShouldBe("5")
      .Run()

  def runTest3(): (Boolean, String) =
    """def main(): int {
         val a = -1;
         if(a >= 0) {
           print(1);
         } else {
           print(0);
         };
         return 0;
       }"""
      .ShouldBe("0")
      .Run()

  def runTest4(): (Boolean, String) =
    """def main(): int {
         val a = 5;
         while( a >= 0 ) {
           a = (a - 1);
         };
         print(a);
         return 0;
       }"""
      .ShouldBe("-1")
      .Run()

  def runTest5(): (Boolean, String) =
    """def main(): int {
         val a = array(1,2,3);
         a[0] = 5;
         print(a[0]);
         return 0;
       }"""
      .ShouldBe("5")
      .Run()

  def runTest6(): (Boolean, String) =
    "def main(): int { val a = 10; print(a[0]); return 0;}"
      .ShouldThrow(new Exception)

  def runTest7(): (Boolean, String) =
    "{def a; a = 5; print(a);}"
      .ShouldThrow(new ParseException(""))

  def runTestBig(): (Boolean, String) = {
    """object Dynamic {
          size: int;
          allocated: int;
          arr: array[int];
          def Dynamic(self: Dynamic) {
              self.arr = array[int][2];
              self.allocated = 2;
              self.size = 0;
          }
          def push(self: Dynamic, value: int) {
              self.arr[self.size] = value;
              self.size += 1;
              if(self.allocated == self.size) {
                  val newsize = (self.allocated * 2);
                  val old = self.arr;
                  self.arr = array[int][newsize];
                  for(val i = 0; i < self.size; i+= 1;) {
                      self.arr[i] = old[i];
                  };
                  self.allocated = newsize;
              };
          }
          def get(self: Dynamic, index: int): int {
              if(index >= self.size) {
                  throw exception("index out of bounds");
              };
              return self.arr[index];
          }
          def print_arr(self: Dynamic) {
              for(val i = 0; i < self.size; i+= 1;) {
                  print(self.arr[i]);
              };
          }
          def compare(self: Dynamic, other: Dynamic): bool {
              val same: bool = true;
              if(self.size != other.size) {return false;};
              for(val i = 0; i < self.size; i+= 1;) {
                  if(self.get(i) != other.get(i)) {same = false;};
              };
              return same;
          }
        }
        def main(): int {
          val a = new Dynamic();
          //print(a.get(8));
          a.push(1);
          a.push(2);
          a.push(3);
          a.print_arr();
          val b = new Dynamic();
          b.push(1);
          b.push(2);
          print(a.compare(b));
          b.push(3);
          print(a.compare(b));
        }"""
      .ShouldBe("true")
      .Run()
  }
}
