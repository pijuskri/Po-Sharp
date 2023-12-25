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

  def runTestGenericFunction1(): (Boolean, String) =
    """def add[T1](a: T1, b: T1): T1 {
      |    return (a+b);
      |}
      |
      |def main(): int {
      |    val b = add[int](2,3);
      |    val a = add[float](2.0,3.0);
      |    print(b);
      |}""".stripMargin
      .ShouldBe("5")
      .Run()

  def runTestGenericInterface1(): (Boolean, String) =
    """object Test[T1] {
      |    toPrint: T1;
      |    def Test(self: Test[T1], toPrint: T1): Test[T1] {
      |        self.toPrint = toPrint;
      |        return self;
      |    }
      |    def __print__(self: Test[T1]) {
      |        print(self.toPrint);
      |        print("\n");
      |    }
      |}
      |
      |def main(): int {
      |    val c = new Test[int](10);
      |    val d = new Test[float](10.5);
      |    print(d);
      |    print("\n");
      |    print(c);
      |}""".stripMargin.stripMargin
      .ShouldBe("10")
      .Run()


  def runTestBig(): (Boolean, String) = {
    """
      object Dynamic {
          size: int;
          allocated: int;
          arr: array[char];

          def Dynamic(self: Dynamic): Dynamic {
              self.arr = array[char][8];
              self.allocated = 8;
              self.size = 0;
              return self;
          }
          def Dynamic(self: Dynamic, arr: array[char]): Dynamic {
              self.Dynamic();
              self.push(arr);
              return self;
          }

          def expand(self: Dynamic, req: int) {
      		val total = (req + self.size);

      		if(total >= self.allocated) {
      			val newsize = self.allocated;
      			while(newsize < (total+1)) {
      				newsize = (newsize * 2);
      			};
      			val old = self.arr;
      			val narr = array[char][newsize];
      			for(val i = 0; i < self.size; i+= 1;) {
      				narr[i] = old[i];
      			};
      			self.arr = narr;
      			free(old);
      			self.allocated = newsize;
      			self.size = total;
      		} else {
                  self.size += req;
      		};
          }
          def push(self: Dynamic, value: char) {
              self.expand(1);
              self.arr[(self.size-1)] = value;
          }
          def push(self: Dynamic, value: array[char]) {
              val oldSize = self.size;
              self.expand(value.size);

              for(val i = 0; i < value.size; i+= 1;) {
                  self.arr[(i + oldSize)] = value[i];
              };
          }

      	def push(self: Dynamic, value: Dynamic) {
             val oldSize = self.size;
             self.expand(value.size);

             for(val i = 0; i < value.size; i+= 1;) {
                 self.arr[(i + oldSize)] = value.arr[i];
             };
          }

          def concat(self: Dynamic, other: Dynamic): Dynamic {
              val ret = self.copy();
              ret.push(other);
              return ret;
          }
          //filter
          def get(self: Dynamic, index: int): char {
              if(index >= self.size) {
                  throw exception("index out of bounds");
              };
              return self.arr[index];
          }
          def print_arr(self: Dynamic) {
              for(val i = 0; i< self.size; i+=1;) {
                  print(self.arr[i]);
                  //print(" ");
              };
              print("\n");
          }
          def copy(self: Dynamic): Dynamic { //still sus cause on stack
              val arr_new = new Dynamic();

              for(val i = 0; i < self.size; i+= 1;) {
                 arr_new.push(self.arr[i]);
              };
              return arr_new;
          }
          def compare(self: Dynamic, other: Dynamic): bool {
              val same: bool = true;
              if(self.size != other.size) {return false;};
              for(val i = 0; i < self.size; i+= 1;) {
                  if(self.get(i) != other.get(i)) {same = false;};
              };
              return same;
          }
          def __add__(self: Dynamic, other: Dynamic): Dynamic {
              return self.concat(other);
          }
          def __print__(self: Dynamic) {
              self.print_arr();
          }
          def not_useful() {
              print("not useful, you're dumb");
          }

      }
        def main(): int {

             val a = new Dynamic();
                a.push(array('a', 'b', 'c'));
                val b = new Dynamic(array('d', 'e', 'f'));
                print(a+b);
        }
        """
      .ShouldBe("abcdef")
      .Run()
  }
}
