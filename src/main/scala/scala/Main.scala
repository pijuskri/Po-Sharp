package scala

import java.io.{File, FileWriter}
import scala.io.Source


object Main extends App {
  val toCompile = readFile("", "toCompile.txt")
  val parsed = Parser.parseInput(toCompile);
  println(parsed);
  println("")
  val asm = ToAssembly.convertMain(parsed);
  println(asm);

  writeToFile(asm, "compiled/", "hello.asm")

  def writeToFile(input: String, directoryPath: String, filename: String): Unit = {
    val directory = new File(directoryPath);
    if (!directory.exists()) directory.mkdir();

    val fileWriter = new FileWriter(new File(directoryPath+filename))
    fileWriter.write(input)
    fileWriter.close()
  }
  def readFile(directoryPath: String, filename: String): String = {
    val source = Source.fromFile(new File(directoryPath+filename))
    val codetxt = source.mkString
    source.close()
    codetxt
  }
}



