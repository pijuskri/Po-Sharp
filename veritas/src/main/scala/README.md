# Veritas

Veritas is a testing framework built to help test PoSharp code. 

It is currently targetting windows only as it uses WSL under the hood, this might change in the future. This may also work
for linux or mac locally but do note that there were issues on the remote pipeline with an Ubuntu image.

## Usage

### Writing a Test

Writing tests is pretty straight forward. For a method to be considered a test it must:

- Be inside the `test` package
- Be inside a class annotated with the `veritas.Veritas.Test` annotation
- Include the word `test` in the method name
- Have a return type of `(Boolean, String)`

The [`PoSharp.scala`](./core/PoSharp.scala) class is created to provide an interface as well as some helper methods to aid in
writing tests. Do note that the framework considers the last printed value to be the value to check, this means that each
snippet must have a print statement.

A simple test might look like this;

```scala
@Test
class test.TestExample {
  def runTest2(): (Boolean, String) =
    """def main(): int {
         val a = 5;
         print(a);
         return 0;
       }"""
      .ShouldBe("5")
      .Run()
}
```

Notice how implicit classes are being used to trim down the needed syntax. These were added later on but in case you do
not want to use them, you can rewrite the test to:

```scala
@Test
class test.TestExample {
  def runTest2(): (Boolean, String) =
    PoSharpScript("""def main(): int {
         val a = 5;
         print(a);
         return 0;
       }""")
      .ShouldBe("5")
      .Run()
}
```

The [`PoSharp.scala`](./core/PoSharp.scala) file contains thorough documentation on all the different methods that it provides
which I will be keeping up to date so be sure to read through the JavaDoc.

### Running the Tests

Running tests takes quite a bit of time as they need to be compiled and executed on a WSL external process, the framework
leverages multithreading to try and combat this.

You can run the tests with `gradle runTests` or run tests + print coverage with `gradle runCoverage`.
If you want to also generate the CodeCov JSON, run `gradle runCoverage -PextraArgs="export"`


## The Framework Explained

I explain a lot of the inner workings [here](https://antoniosbarotsis.github.io/Blog/posts/posharp/).

- `Veritas.scala`: Main file. Detects and executes all tests.
- `Coverage.scala`: Calculates and prints the coverage after the tests are executed, also generates a CodeCov report.
- `PoSharp.scala`: Contains helper classes and assertions for writing the tests.