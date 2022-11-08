
<div id="top"></div>
<!--
*** Thanks for checking out the Best-README-Template. If you have a suggestion
*** that would make this better, please fork the repo and create a pull request
*** or simply open an issue with the tag "enhancement".
*** Don't forget to give the project a star!
*** Thanks again! Now go create something AMAZING! :D
-->

<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->

<p align="center">
    <img src='docs/logo.png' alt="">
</p>

[![Contributors][contributors-shield]][contributors-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/pijuskri/Po-Sharp/Build%20and%20Test?label=Build%20and%20Test&style=for-the-badge)](https://github.com/pijuskri/Po-Sharp/actions/workflows/workflow.yml)


<h2 align="center">Po#</h2>
<div>
  <p align="center">
    Custom language frontend for LLVM IR, written in Scala
    <br />
    <br />
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
    </li>
    <li><a href="#prerequisites">Prerequisites</a></li>
    <li><a href="#language">Language specification</a></li>
    <li><a href="#progress">Progress</a></li>
    <li><a href="#todo">To do</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project
<div id="about-the-project"></div>

A testing ground for my knowledge of unix, make, scala, compilers and assembly. I called it Po#
cause my friend suggested something like P#, but that is already a language,
so i chose Po# as it reminds me of potatoes :). I will also base the language on C# in some ways.

I did a compiler course in university, but we actually wrote a interpreter there.
The language was quite functional and of limited syntax. I tried to extend it to a different
syntax and add more useful syntactic sugar (it didn't even have loops). However, it was hard to
work with at the end. Therefore, I decided to make a new base, this time also making a compiler.

The intention is to make this language a combination of features i really like from many languages,
like scala, java, c# and python. The main focus currently will be c++ like, as i find that easiest
to translate to assembly.

### Built With
<div id="built-with"></div>

* [Scala](https://www.scala-lang.org/)
* [FastParse](https://github.com/com-lihaoyi/fastparse)
* [LLVM](https://llvm.org/)

<!-- GETTING STARTED -->
### Prerequisites
<div id="prerequisites"></div>

* [Ubuntu 18.04 or newer]() and/or [WSL](https://docs.microsoft.com/en-us/windows/wsl/install)
* [JDK 13+](https://www.oracle.com/java/technologies/downloads/)
* [Scala 2.13+](https://www.scala-lang.org/download/)
* [Gradle 7.5](https://gradle.org/install/)
* [GCC](https://gcc.gnu.org/)
* [Clang 15+](https://releases.llvm.org/)
* [LLVM 15+](https://releases.llvm.org/download.html)

### Getting Started
<div id="getting-started"></div>


There is also an option to compile with a single command using sbt. Just run `make full`
in the main directory

For now the code that is interpreted can be typed in the Main object. The assembly file
will be generated in `compiled/hello.asm` (do not ask why I named it that).

With IntelliJ
* Run `Main.scala`
* In root directory call `make`

With sbt
* In root directory call `make full`

[//]: # (TODO Does this still work? Probably a good idea to use gradle instead)

A [testing framework](./veritas/src/main/scala/core/) is also included in the project.
To run the language tests do `gradle runTests`. The documentation can be found
[here](./veritas/src/main/scala/README.md).

### Language specification
<div id="language"></div>

#### [Full guide to the language](docs/Guide.md)

```scala 
//recursive fibonacci implementation
def main(): int {
  val a = 9;
  print(fib(a));
  return 0;
}
def fib(n: int): int {
  if(n <= 1) {return n;};
  return (fib((n-1)) + fib((n-2)));
}
```

#### [More examples](docs/examples.txt) 


### Progress
<div id="progress"></div>

* Integer arithmetic (with forced parenthesis for subexpressions)
* variable definition and assignment
* print function
* if/else (&&, ||, ==, >, <, !, !=, >=, <=), forced parenthesis
* While, for loops
* Arrays(default values, size access)
* Types
* Functions
* Float support
* Strings
* Enums
* Objects
* runtime exceptions
* multiple files

### To do
<div id="todo"></div>

#### Major

* Tuples
* Extension methods
* lambda functions
* Generics
* Object inheritance
* library functions
* typeof
* Garbage collector
* packages
* File i/o
* Optimisation

#### Minor

* Structs
* ref/out
* Type alias
* Interface union
* String manipulation
* user input
* online demo for posharp
* Input filename (perhaps make compiled version for release)


<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/pijuskri/Po-Sharp.svg?style=for-the-badge
[contributors-url]: https://github.com/pijuskri/Po-Sharp/graphs/contributors
[issues-shield]: https://img.shields.io/github/issues/pijuskri/Po-Sharp.svg?style=for-the-badge
[issues-url]: https://github.com/pijuskri/Po-Sharp/issues
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/in/pijus-krisiuk%C4%97nas-66177715b/

[license-shield]: https://img.shields.io/github/license/pijuskri/Po-Sharp.svg?style=for-the-badge
[license-url]: https://github.com/pijuskri/Po-Sharp/blob/master/LICENSE.md
[forks-shield]: https://img.shields.io/github/forks/github_username/repo_name.svg?style=for-the-badge
[forks-url]: https://github.com/github_username/repo_name/network/members
[stars-shield]: https://img.shields.io/github/stars/github_username/repo_name.svg?style=for-the-badge
[stars-url]: https://github.com/github_username/repo_name/stargazers
[product-screenshot]: docs/logo.png