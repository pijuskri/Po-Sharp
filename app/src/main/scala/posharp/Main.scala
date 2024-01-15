package posharp
import java.io.{File, FileWriter}
import java.nio.file.Paths
import scala.io.{AnsiColor, Source}

object Constants {
  val FileExtension = ".txt"
}
//input_sourceDir: Option[String]
@main def Main(): Unit = {
  val sourceDir = "po_src"//input_sourceDir.getOrElse("po_src")

  val files = recursiveListFiles(new File(sourceDir), "ignore").toList.filter(x => x.getName.contains(Constants.FileExtension))
  val sourceDirPath = Paths.get(sourceDir)
  val declarations: Map[String, (ToAssembly, Expr.TopLevel)] = files.map(file => {
    val toCompile = readFile(file)
    var relative_name = sourceDirPath.relativize(file.toPath).toFile.getPath.split(Constants.FileExtension)(0)
    relative_name = relative_name.replace("\\", "/")

    val parsed = Parser.parseInput(toCompile, relative_name.replace("/", "_")).get;
    //println(Util.prettyPrint(parsed))
    val top = parsed match {
      case x: Expr.TopLevel => x
      case _ => throw new Exception("unexpected type in top level")
    }

    (relative_name -> (new ToAssembly(relative_name), top))
  }).toMap
  //declaration step
  declarations.foreach(x => {
    val code = x._2._2
    val converter = x._2._1
    converter.declarationPass(code)
  })
  declarations.foreach(x => {
    val file = x._1
    val code = x._2._2
    val converter = x._2._1
    var asm = "";

    try {
      asm = converter.convertMain(code, declarations.map(x => x._1 -> x._2._2).filter(x => x._1 != file));
      //asm += StringCode.stringCode;
    }
    catch {
      case x: Exception => {
        println(AnsiColor.RED + s"Compilation exception in \"$file\": ${x.getMessage} ${x.getStackTrace.mkString("\n")}" + AnsiColor.RESET);
        sys.exit(1);
      }
    }

    println("")
    writeCompiled(asm, "compiled/", file)
  })
}
def writeCompiled(asm: String, directoryPath: String, file: String): Unit = {
  val flatFile = file.split("/").last + ".ll"
  writeToFile(asm, directoryPath, flatFile)
}

def writeToFile(input: String, directoryPath: String, filename: String): Unit = {
  val directory = new File(directoryPath);
  if (!directory.exists()) directory.mkdir();

  val fileWriter = new FileWriter(new File(directoryPath + filename))
  fileWriter.write(input)
  fileWriter.close()
}
def readFile(src: File): String = {
  val source = Source.fromFile(src)
  val codetxt = source.mkString
  source.close()
  codetxt
}

def recursiveListFiles(f: File, ignore: String): Array[File] = {
  if (!f.exists()) {
    return Array()
  }

  if (f.isFile) return Array(f)
  val these = f.listFiles
  these ++ these.filter(x => x.isDirectory && x.getName != ignore).flatMap(x => recursiveListFiles(x, ignore))
}

object StringCode {
  val stringCode: String = """; The actual type definition for our 'String' type.
                     |%String = type {
                     |    i8*,     ; 0: buffer; pointer to the character buffer
                     |    i32,     ; 1: length; the number of chars in the buffer
                     |    i32,     ; 2: maxlen; the maximum number of chars in the buffer
                     |    i32      ; 3: factor; the number of chars to preallocate when growing
                     |}
                     |
                     |define private fastcc void @String_Create_Default(%String* %this) nounwind {
                     |    ; Initialize 'buffer'.
                     |    %1 = getelementptr %String, %String* %this, i32 0, i32 0
                     |    store i8* null, i8** %1
                     |
                     |    ; Initialize 'length'.
                     |    %2 = getelementptr %String, %String* %this, i32 0, i32 1
                     |    store i32 0, i32* %2
                     |
                     |    ; Initialize 'maxlen'.
                     |    %3 = getelementptr %String, %String* %this, i32 0, i32 2
                     |    store i32 0, i32* %3
                     |
                     |    ; Initialize 'factor'.
                     |    %4 = getelementptr %String, %String* %this, i32 0, i32 3
                     |    store i32 16, i32* %4
                     |
                     |    ret void
                     |}
                     |
                     |
                     |define private fastcc void @String_Delete(%String* %this) nounwind {
                     |    ; Check if we need to call 'free'.
                     |    %1 = getelementptr %String, %String* %this, i32 0, i32 0
                     |    %2 = load i8*, i8** %1
                     |    %3 = icmp ne i8* %2, null
                     |    br i1 %3, label %free_begin, label %free_close
                     |
                     |free_begin:
                     |    call void @free(i8* %2)
                     |    br label %free_close
                     |
                     |free_close:
                     |    ret void
                     |}
                     |
                     |define private fastcc void @String_Resize(%String* %this, i32 %value) {
                     |    ; %output = malloc(%value)
                     |    %output = call i8* @malloc(i32 %value)
                     |
                     |    ; todo: check return value
                     |
                     |    ; %buffer = this->buffer
                     |    %1 = getelementptr %String, %String* %this, i32 0, i32 0
                     |    %buffer = load i8*, i8** %1
                     |
                     |    ; %length = this->length
                     |    %2 = getelementptr %String, %String* %this, i32 0, i32 1
                     |    %length = load i32, i32* %2
                     |
                     |    ; memcpy(%output, %buffer, %length)
                     |    %3 = call i8* @memcpy(i8* %output, i8* %buffer, i32 %length)
                     |
                     |    ; free(%buffer)
                     |    call void @free(i8* %buffer)
                     |
                     |    ; this->buffer = %output
                     |    store i8* %output, i8** %1
                     |
                     |    ;this->maxlen = %value (value that was passed into @malloc is the new maxlen)
                     |    %4 = getelementptr %String, %String* %this, i32 0, i32 2
                     |    store i32 %value, i32* %4
                     |    ret void
                     |}
                     |
                     |define private fastcc void @String_Add_Char(%String* %this, i8 %value) {
                     |    ; Check if we need to grow the string.
                     |    %1 = getelementptr %String, %String* %this, i32 0, i32 1
                     |    %length = load i32, i32* %1
                     |    %2 = getelementptr %String, %String* %this, i32 0, i32 2
                     |    %maxlen = load i32, i32* %2
                     |    ; if length == maxlen:
                     |    %3 = icmp eq i32 %length, %maxlen
                     |    br i1 %3, label %grow_begin, label %grow_close
                     |
                     |grow_begin:
                     |    %4 = getelementptr %String, %String* %this, i32 0, i32 3
                     |    %factor = load i32, i32* %4
                     |    %5 = add i32 %maxlen, %factor
                     |    call void @String_Resize(%String* %this, i32 %5)
                     |    br label %grow_close
                     |
                     |grow_close:
                     |    %6 = getelementptr %String, %String* %this, i32 0, i32 0
                     |    %buffer = load i8*, i8** %6
                     |    %7 = getelementptr i8, i8* %buffer, i32 %length
                     |    store i8 %value, i8* %7
                     |    %8 = add i32 %length, 1
                     |    store i32 %8, i32* %1
                     |
                     |    ret void
                     |}""".stripMargin
}




