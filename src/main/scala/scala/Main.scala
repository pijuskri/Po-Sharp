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

  case class LessThan(left: Expr, right: Expr) extends Expr
  case class MoreThan(left: Expr, right: Expr) extends Expr
  case class Equals(left: Expr, right: Expr) extends Expr
  case class If(condition: Expr, ifTrue: Expr.Block, ifFalse: Expr) extends Expr
  case class And( conditions: List[Expr] ) extends Expr
  case class Or( conditions: List[Expr] ) extends Expr
  case class True() extends Expr
  case class False() extends Expr
  //case class RetIf(condition: Expr, ifTrue: Expr, ifFalse: Expr) extends Expr

  case class Print(value: Expr) extends Expr
  case class SetVal(variable: Expr, value: Expr) extends Expr
  case class DefVal(variable: Expr) extends Expr
  case class Block(lines: List[Expr]) extends Expr
  case class ExtendBlock(lines: List[Expr]) extends Expr
  //case class Local(name: String, assigned: Expr, body: Expr) extends Expr
  //case class Func(argNames: Seq[String], body: Expr) extends Expr
  //case class Call(expr: Expr, args: Seq[Expr]) extends Expr

  case class Nothing() extends Expr
}

object Parser {

  def block[_: P] = P("{" ~ line.rep(0) ~ "}").map ( lines => lines.foldLeft(List(): List[Expr])( (acc, el) => el match {
    case Expr.ExtendBlock(sub) => acc ::: sub;
    case _ => acc :+ el;
  } )).map(x=>Expr.Block(x))
  def line[_: P]:P[Expr] = P(expr ~/ ";")
  def expr[_: P]: P[Expr] = P( defAndSetVal | defVal | setVal | IfOp | print)

  def prefixExpr[_: P]: P[Expr] = P( parens | number | ident | str)

  def defVal[_: P] = P("def " ~ ident).map(x => Expr.DefVal(x))
  def setVal[_: P] = P(ident ~ "=" ~ prefixExpr).map(x => Expr.SetVal(x._1, x._2))
  def defAndSetVal[_: P] = P( "def " ~ ident ~ "=" ~ prefixExpr).map(x => Expr.ExtendBlock(List(Expr.DefVal(x._1), Expr.SetVal(x._1, x._2))))

  def parens[_: P] = P("(" ~ binOp ~ ")")

  /*
  def binOp[_: P] = P(prefixExpr ~ StringIn("+", "-", "*", "/", "==", ">", "<").! ~/ prefixExpr).map({
    case (l, "+", r) => Expr.Plus(l, r)
    case (l, "-", r) => Expr.Minus(l, r)
    case (l, "*", r) => Expr.Mult(l, r)
    case (l, "/", r) => Expr.Div(l, r)
    case (l, "==", r) => Expr.Equals(l,r)
    case (l, "<", r) => Expr.LessThan(l,r)
    case (l, ">", r) => Expr.MoreThan(l,r)
    case _ => throw new ParseException("not bin p[")
  })
   */

  def binOp[_: P] = P( prefixExpr ~ ( StringIn("+", "-", "*", "/", "==", ">", "<").! ~/ prefixExpr).rep(1)).map(list => parseBinOpList(list._1, list._2.toList))

  def parseBinOpList(initial: Expr, rest: List[(String, Expr)]): Expr = {
    rest.foldLeft(initial) {
      case (left, (operator, right)) =>
        operator match {
          case "+" => Expr.Plus(left, right)
          case "-" => Expr.Minus(left, right)
          case "*" => Expr.Mult(left, right)
          case "/" => Expr.Div(left, right)
          case "==" => Expr.Equals(left, right)
          case "<" => Expr.LessThan(left, right)
          case ">" => Expr.MoreThan(left, right)
        }
    }
  }

  def IfOp[_: P]: P[Expr.If] = P("if" ~ condition ~ block ~ elseOp.? ) .map{
    case (cond, ex_true, Some(ex_else)) => Expr.If(cond, ex_true, ex_else);
    case (cond, ex_true, None) => Expr.If(cond, ex_true, Expr.Block(List()));
  }

  //binOp | orCond |
  def condition[_: P]: P[Expr] = P("(" ~/ (condOp | binOp) ~ ")")
  /*
  def andCond[_: P]: P[Expr.And] = P(condition ~ ("&&" ~/ condition).rep).map((conds) => Expr.And(conds._1 +: conds._2.toList ))
  def orCond[_: P]: P[Expr.Or] = P(condition ~ ("||" ~/ condition).rep).map((conds) => Expr.Or(conds._1 +: conds._2.toList ))
   */
  /*
  rest._2.toList.foldLeft((List(first), "")) {
    case (acc, (operator, right)) =>
      if(operator != acc._2 && acc._2 != "") throw new Exception("different logical operators in the same line")
      operator match {
        case "&&" => (acc._1 :+ right,"&&")
        case "-" => (acc._1 :+ right,"&&")
        case "==" => Expr.Equals(left, right)
      }
  }
   */
  def condOp[_: P]: P[Expr] = P(condition ~ ("&&" | "||").! ~/ condition ~ (("&&" | "||") ~/ condition).rep).map{
    case (first, "&&", second, rest) => Expr.And(List(first, second) ::: rest.toList)
    case (first, "||", second, rest) => Expr.Or(List(first, second) ::: rest.toList)
  }

  def elseOp[_: P] = P("else" ~ block)

  def str[_: P]: P[Expr] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(Expr.Str)

  def ident[_: P]: P[Expr.Ident] = P(CharIn("a-zA-Z_") ~~ CharsWhileIn("a-zA-Z0-9_", 0)).!.map(Expr.Ident)

  def number[_: P]: P[Expr.Num] = P(CharsWhileIn("0-9", 1)).!.map(x => Expr.Num(Integer.parseInt(x)))

  def print[_: P]: P[Expr.Print] = P("print(" ~ ident ~ ")").map(Expr.Print)

  class ParseException(s: String) extends RuntimeException(s)
  val reservedKeywords = List("def", "and", "or");
  def checkForReservedKeyword(input: Expr.Ident): Unit ={
    if(reservedKeywords.contains(input.name)) throw new ParseException("parsing fail");
  }

  def parseInput(input: String): Expr = {
    val parsed = fastparse.parse(input, block(_));
    parsed match {
      case Parsed.Success(expr, n) => expr;
      case t: Parsed.Failure => {println(t.trace().longAggregateMsg); throw new ParseException("parsing fail");}
      case _ => throw new ParseException("parsing fail")
    }
  }
}

object Main extends App {
  //def divMul[_: P] = P( prefixExpr ~ CharIn("*/").! ~/ prefixExpr ).map( token => Expr.BinOp(token._2, token._1, token._3))
  //def addSub[_: P] = P( prefixExpr ~ CharIn("+\\-").! ~/ prefixExpr ).map( token => Expr.BinOp(token._2, token._1, token._3))
  //{def a = 3; def c = 5; def b = 0; if( (a == 2) || (c == 5) ) {b = 2;} else {b = 3;};}
  val parsed = Parser.parseInput("{def a = 3; def c = 5; def b = 0; if( (a > 2) || (c < 6)) {b = 2;} else {b = 3;};}");
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
  var lineNr = 0;
  var ifCounter = 0;
  var subconditionCounter: Int = 0;
  private def convert(input: Expr, reg: List[String], env: Map[String, Int]): String = {
    input match {
      case Expr.Num(value) => s"mov ${reg.head}, 0${value}d\n"
      case Expr.Plus(left, right) => binOpTemplate(left, right, "add", reg, env)
      case Expr.Minus(left, right) => binOpTemplate(left, right, "sub", reg, env)
      case Expr.Mult(left, right) => mulTemplate(left, right, "imul", reg, env)
      case Expr.Ident(name) => s"mov ${reg.head}, ${lookup(name, env)}\n"
      //case Expr.Div(left, right) => mulTemplate(left, right, "idiv", reg)
      case Expr.Block(lines) => {
        if(lines.isEmpty) return "";
        var newenv = env;
        val defstring: String = lines.head match {
          case Expr.SetVal(Expr.Ident(name), value) => convert(value, reg, env) + setval(name, env);
          case Expr.DefVal( Expr.Ident(name)) => newenv = newVar(name, newenv); ""
          case Expr.Print(toPrint) => printInterp(toPrint, env);
          case Expr.If(condition, ifTrue, ifFalse) => {
            /*
            val ret = convert(condition, reg, env) + s"if_${ifCounter}_false:\n" + convert(ifFalse, reg, env) +
              s"jmp if_${ifCounter}_end\n" + s"if_${ifCounter}_true:\n" + convert(ifTrue, reg, env) + s"if_${ifCounter}_end:\n"
             */
            val trueLabel = s"if_${ifCounter}_true"
            val falseLabel = s"if_${ifCounter}_false"
            val endLabel = s"if_${ifCounter}_end"
            val ret = convertCondition(condition, reg, env, orMode = false, trueLabel, falseLabel) + s"${trueLabel}:\n" + convert(ifTrue, reg, env) +
              s"jmp ${endLabel}\n" + s"${falseLabel}:\n" + convert(ifFalse, reg, env) + s"${endLabel}:\n"
            ifCounter += 1;
            ret
          }
          case _ => throw new Exception(lines.head.toString + " should not be in block lines")
        }
        if(lines.tail.nonEmpty) {
          lineNr+=1;
          defstring + convert(Expr.Block(lines.tail), defaultReg, newenv)
        }
        else defstring;
      }


      case Expr.Nothing() => "";
      case _ => throw new Exception ("not interpreted yet :(");
    }
  }
  private def convertCondition(input: Expr, reg: List[String], env: Map[String, Int], orMode: Boolean, trueLabel: String, falseLabel: String): String = {
    def compare(left: Expr, right: Expr): String = binOpTemplate(left, right, "cmp", reg, env)
    val newtrueLabel = s"cond_${subconditionCounter}_true"
    val newfalseLabel = s"cond_${subconditionCounter}_false"
    val ret = input match {
      case Expr.Equals(left, right) => {
        compare(left, right) + ( if(orMode) s"je ${trueLabel}\n" else s"jne ${falseLabel}\n" )
      }
      case Expr.LessThan(left, right) => {
        compare(left, right) + ( if(orMode) s"jl ${trueLabel}\n" else s"jge ${falseLabel}\n" )
      }
      case Expr.MoreThan(left, right) => {
        compare(left, right) + ( if(orMode) s"jg ${trueLabel}\n" else s"jle ${falseLabel}\n" )
      }
      case Expr.And(list) => {
        subconditionCounter += 1;
        list.foldLeft("")((acc, subcond) => acc + convertCondition(subcond, reg, env, orMode = false, newtrueLabel, newfalseLabel)) +
          s"${newtrueLabel}:\n" + s"jmp ${trueLabel}\n" + s"${newfalseLabel}:\n" + s"jmp ${falseLabel}\n"
      }
      case Expr.Or(list) => {
        subconditionCounter += 1;
        list.foldLeft("")((acc, subcond) => acc + convertCondition(subcond, reg, env, orMode = true, newtrueLabel, newfalseLabel)) +
          s"${newtrueLabel}:\n" + s"jmp ${trueLabel}\n" + s"${newfalseLabel}:\n" + s"jmp ${falseLabel}\n"
      }
    }
    ret
  }
  //private def conditionParse(input: Expr, reg: List[String], env: Map[String, Int], )
  def setval(name: String, env: Map[String, Int]): String = {
    s"mov qword ${lookup(name, env)}, rax\n"
  }

  def lookup(tofind: String, env: Map[String, Int]): String = s"[rsp+${lookupOffset(tofind, env)}]"
  def lookupOffset(tofind: String, env: Map[String, Int]): Int = env.get(tofind) match {
    case Some(v) => v
    case None => throw new Exception(s"variable \"${tofind}\" undefined")
  }
  def newVar(name: String, env: Map[String, Int]) : Map[String, Int] = {
    if(env.contains(name)) throw new Exception(s"variable \"${name}\" already defined")
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

