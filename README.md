
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
[![Contributors][contributors-shield]][contributors-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]

<h2 align="center">Po#</h2>
<div>
  <p align="center">
    Custom language compiler to X86_64 nasm assembly, written in Scala
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

<!-- GETTING STARTED -->
### Prerequisites
<div id="prerequisites"></div>

* [JDK 13+](https://www.oracle.com/java/technologies/downloads/)
* [Scala 2.13](https://www.scala-lang.org/download/)
* [SBT 1.6.1](https://www.scala-sbt.org/download.html)
* [Ubuntu 18.04]() and/or [WSL](https://docs.microsoft.com/en-us/windows/wsl/install)

### Getting Started
<div id="getting-started"></div>

Currently, I run the scala compiler through IntelliJ. The conversion from
assembly to binary is handled by a makefile, that i run in WSL with ubuntu 18;
I will add a single command for running everything through cli later.

For now the code that is interpreted can be typed in the Main object. The assembly file
will be generated in `compiled/hello.asm` (do not ask why I named it that).

* Run `Main.scala`
* In root directory call `make`

### Program example

``` 
{
    /*
     Functionless power function b^a
     */
    def a = 10;
    def b = 2;
    def c = b;
    while (a > 1) {
        b = (b * c);
        a = (a - 1);
    };
    print(b);
}
```

### Progress
<div id="progress"></div>

* Integer arithmetic (with forced parenthesis for subexpressions)
* variable definition and assignment
* print function
* if/else (&&, ||, ==, >, <, !, !=, >=, <=), forced parenthesis
* While loops

### To do
<div id="todo"></div>

* Arrays
* Functions
* Loops (for)
* Types
* Float support
* Strings
* Garbage collector
* Objects/structs
* Optimisations with register/stack combinations


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
[product-screenshot]: images/screenshot.png