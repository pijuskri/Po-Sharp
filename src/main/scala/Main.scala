package scala

import fastparse.MultiLineWhitespace._
import fastparse._

import java.io.{File, FileWriter}


sealed trait Expr
object Expr{
  case class Str(s: String) extends Expr
  case class Ident(name: String) extends Expr
  case class Num(value: Int) extends Expr
  case class Plus(left: Expr, right: Expr) extends Expr
  case class Minus(left: Expr, right: Expr) extends Expr
  case class Mult(left: Expr, right: Expr) extends Expr
  case class Div(left: Expr, right: Expr) extends Expr
  case class Print(value: Expr) extends Expr
  case class SetVal(variable: Expr, value: Expr) extends Expr
  case class DefVal(variable: Expr) extends Expr
  case class Block(lines: List[Expr]) extends Expr
  //case class Local(name: String, assigned: Expr, body: Expr) extends Expr
  //case class Func(argNames: Seq[String], body: Expr) extends Expr
  //case class Call(expr: Expr, args: Seq[Expr]) extends Expr
}

object Parser {

  def block[_: P] = P( "{" ~ line.rep(0) ~ "}").map(x=>Expr.Block(x.toList))
  def line[_: P] = P(expr ~ ";")
  def expr[_: P]: P[Expr] = P(defVal | setVal | print)

  def prefixExpr[_: P]: P[Expr] = P(parens | number | str)

  def defVal[_: P] = P("def " ~ ident).map(x => Expr.DefVal(x))
  def setVal[_: P] = P(ident ~ "=" ~ prefixExpr).map(x => Expr.SetVal(x._1, x._2))

  def parens[_: P] = P("(" ~ binOp ~ ")")
  def binOp[_: P] = P(prefixExpr ~ CharIn("+\\-*/").! ~/ prefixExpr).map({
    case (l, "+", r) => Expr.Plus(l, r)
    case (l, "-", r) => Expr.Minus(l, r)
    case (l, "*", r) => Expr.Mult(l, r)
    case (l, "/", r) => Expr.Div(l, r)
    case _ => throw new ParseException("not bin p[")
  })

  def str[_: P]: P[Expr] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(Expr.Str)

  def ident[_: P]: P[Expr.Ident] = P(CharIn("a-zA-Z_") ~~ CharsWhileIn("a-zA-Z0-9_", 0)).!.map(Expr.Ident)

  def number[_: P]: P[Expr.Num] = P(CharsWhileIn("0-9", 0)).!.map(x => Expr.Num(Integer.parseInt(x)))

  def print[_: P]: P[Expr.Print] = P("print(" ~ ident ~ ")").map(Expr.Print)

  class ParseException(s: String) extends RuntimeException(s)

  def parseInput(input: String): Expr = {
    val parsed = fastparse.parse(input, block(_));
    parsed match {
      case Parsed.Success(expr, n) => expr;
      case Parsed.Failure(s, m, ext) => {println(ext.startIndex); throw new ParseException("parsing fail");}
      case _ => throw new ParseException("parsing fail")
    }
  }
}

object Main extends App {
  //def divMul[_: P] = P( prefixExpr ~ CharIn("*/").! ~/ prefixExpr ).map( token => Expr.BinOp(token._2, token._1, token._3))
  //def addSub[_: P] = P( prefixExpr ~ CharIn("+\\-").! ~/ prefixExpr ).map( token => Expr.BinOp(token._2, token._1, token._3))

  val parsed = Parser.parseInput("{def a; a = 5; print(a); a = 3;}");
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
}

object ToAssembly {
  val defaultReg = List("rax", "rdi", "rsi", "rdx", "rcx", "r8", "r9", "r10", "r11")
  def convertMain(input: Expr): String = {
    var converted =
      """ global main
        | extern printf
        | section .text
        |main:
        | sub rsp, 256
        |""".stripMargin;
    converted += convert(input, defaultReg, Map() );
    converted +=
        """mov rdi, format_num
        |mov rsi, rax
        |xor rax, rax
        |call printf
        |""".stripMargin
    converted += "add rsp, 256\n"
    converted += "  mov rax, 0\n  ret\n"
    converted += "format_num:\n        db  \"%d\", 10, 0"
    converted
  }
  private def convert(input: Expr, reg: List[String], env: Map[String, Int]): String = {
    input match {
      case Expr.Num(value) => s"mov ${reg.head}, 0${value}d\n"
      case Expr.Plus(left, right) => binOpTemplate(left, right, "add", reg, env)
      case Expr.Minus(left, right) => binOpTemplate(left, right, "sub", reg, env)
      case Expr.Mult(left, right) => mulTemplate(left, right, "imul", reg, env)
      case Expr.Ident(name) => s"mov ${reg.head}, ${lookup(name, env)}\n"
      //case Expr.Div(left, right) => mulTemplate(left, right, "idiv", reg)
      case Expr.Block(lines) => {
        var newenv = env;
        val defstring: String = lines.head match {
          case Expr.SetVal(Expr.Ident(name), value) => convert(value, reg, env) + setval(name, env);
          case Expr.DefVal( Expr.Ident(name)) => newenv = newVar(name, newenv); ""
          case Expr.Print(toPrint) => printInterp(toPrint, env);
          case _ => throw new Exception(lines.head.toString + " should not be in block lines")
        }
        if(lines.tail.nonEmpty) defstring + convert(Expr.Block(lines.tail), defaultReg, newenv)
        else defstring;
      }
      case _ => throw new Exception ("not interpreted yet :(");
    }
  }
  def setval(name: String, env: Map[String, Int]): String = {
    s"mov qword ${lookup(name, env)}, rax\n"
  }

  def lookup(tofind: String, env: Map[String, Int]): String = s"[rsp+${lookupOffset(tofind, env)}]"
  def lookupOffset(tofind: String, env: Map[String, Int]): Int = env.get(tofind) match {
    case Some(v) => v
    case None => throw new Exception("no variable defined")
  }
  def newVar(name: String, env: Map[String, Int]) : Map[String, Int] = {
    val newOffset = if(env.isEmpty) 0 else env.values.max
    env + (name -> (newOffset + 8))
  }
  def mulTemplate(left: Expr, right: Expr, command: String, reg: List[String], env: Map[String, Int]): String = {
    val leftout = convert(left, reg, env);
    val rightout = convert(right, reg.tail, env);
    leftout + rightout + s"${command} ${reg.tail.head}\n";
  }
  def binOpTemplate(left: Expr, right: Expr, command: String, reg: List[String], env: Map[String, Int]): String = {
    val leftout = convert(left, reg, env);
    val rightout = convert(right, reg.tail, env);
    leftout + rightout + s"${command} ${reg.head}, ${reg.tail.head}\n";
  }
  def printTemplate(format: String): String = {
      "mov rdi, " + format +
      """
      |mov rsi, rax
      |xor rax, rax
      |call printf
      |""".stripMargin
  }
  def printInterp(toPrint: Expr, env: Map[String, Int]): String = toPrint match {
      case Expr.Num(value) => s"mov rax, 0${value}d\n" + printTemplate("format_num");
      case Expr.Ident(value) => s"mov rax, ${lookup(value, env)}\n" + printTemplate("format_num");
      case _ => throw new Exception(toPrint.toString + " not recognized in print")
  }
}

