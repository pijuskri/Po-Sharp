package posharp
import java.io.{File, FileWriter}
import java.nio.file.Paths
import scala.io.{AnsiColor, Source}

package object Constants {
  val FileExtension = ".txt"
}


object Main extends App {
  var sourceDir = "po_src"

  if (args.length > 0) {
    sourceDir = args(0)
  }
  val files = recursiveListFiles(new File(sourceDir)).toList.filter(x=>x.getName.contains(Constants.FileExtension))
  val sourceDirPath = Paths.get(sourceDir)
  val declarations: Map[String, Expr.TopLevel] = files.map(file => {
    val toCompile = readFile(file)
    val parsed = Parser.parseInput(toCompile);
    val top = parsed match {
      case x: Expr.TopLevel => x
      case _ => throw new Exception("unexpected type in top level")
    }
    var relative_name = sourceDirPath.relativize(file.toPath).toFile.getPath.split(Constants.FileExtension)(0)
    relative_name = relative_name.replace("\\", "/")
    (relative_name -> top)
  }).toMap
  declarations.foreach(x => {
    val file = x._1
    val code = x._2
    var asm = "";

    try {
       asm = ToAssembly.convertMain(code, file, declarations.filter(x => x._1 != file));
    }
    catch {
      case x: Exception => {
        println( AnsiColor.RED + s"Compilation exception in \"$file\": ${x.getMessage}" + AnsiColor.RESET);
        sys.exit(1);
      }
    }
    println("")
    writeCompiled(asm, "compiled/", file)
  })

  def writeCompiled(asm: String, directoryPath: String, file: String): Unit = {
    val flatFile = file.split("/").last + ".ll"
    writeToFile(asm, directoryPath, flatFile)
  }

  def writeToFile(input: String, directoryPath: String, filename: String): Unit = {
    val directory = new File(directoryPath);
    if (!directory.exists()) directory.mkdir();

    val fileWriter = new FileWriter(new File(directoryPath+filename))
    fileWriter.write(input)
    fileWriter.close()
  }
  def readFile(src: File): String = {
    val source = Source.fromFile(src)
    val codetxt = source.mkString
    source.close()
    codetxt
  }
  def recursiveListFiles(f: File): Array[File] = {
    if(f.isFile) return Array(f)
    val these = f.listFiles
    these ++ these.filter(x=>x.isDirectory).flatMap(x=>recursiveListFiles(x))
  }
}



