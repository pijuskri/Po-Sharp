package scala

import fastparse.JavaWhitespace._
import fastparse._

import java.io.{File, FileWriter}
import scala.io.Source


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
  case class Not(condition: Expr) extends Expr
  //case class RetIf(condition: Expr, ifTrue: Expr, ifFalse: Expr) extends Expr

  case class Print(value: Expr) extends Expr
  case class SetVal(variable: Expr, value: Expr) extends Expr
  case class DefVal(variable: Expr) extends Expr
  case class Block(lines: List[Expr]) extends Expr
  case class ExtendBlock(lines: List[Expr]) extends Expr
  case class While(condition: Expr, execute: Expr.Block) extends Expr

  case class GetArray(array: Ident, index: Int) extends Expr
  case class Array(startPointer: StackVar, size: Int) extends Expr
  case class DefineArray(size: Int) extends Expr

  case class StackVar(offset: Int) extends Expr
  //case class Local(name: String, assigned: Expr, body: Expr) extends Expr
  //case class Func(argNames: Seq[String], body: Expr) extends Expr
  //case class Call(expr: Expr, args: Seq[Expr]) extends Expr

  case class Nothing() extends Expr
}

object Parser {

  def block[_: P] = P("{" ~/ line.rep(0) ~ "}").map ( lines => lines.foldLeft(List(): List[Expr])( (acc, el) => el match {
    case Expr.ExtendBlock(sub) => acc ::: sub;
    case _ => acc :+ el;
  } )).map(x=>Expr.Block(x))
  def line[_: P]:P[Expr] = P(expr ~/ ";")
  def expr[_: P]: P[Expr] = P( arrayDef | defAndSetVal | defVal | setVal | IfOp | whileLoop | print)

  def prefixExpr[_: P]: P[Expr] = P( parens | getArray | number | ident | constant | str)

  def defVal[_: P] = P("def " ~ ident).map(x => Expr.DefVal(x))
  def setVal[_: P] = P(ident ~ "=" ~ prefixExpr).map(x => Expr.SetVal(x._1, x._2))
  def defAndSetVal[_: P] = P( "def " ~ ident ~ "=" ~ prefixExpr).map(x => Expr.ExtendBlock(List(Expr.DefVal(x._1), Expr.SetVal(x._1, x._2))))

  def arrayDef[_: P] = P("def_arr " ~/ ident ~ "[" ~ number ~ "]").map(x => {
    if(x._2.value < 0) throw new ParseException("negative array size");
    Expr.ExtendBlock(List(
      Expr.DefVal(x._1),
      Expr.SetVal(x._1, Expr.DefineArray(x._2.value))
    ));
  })
  def getArray[_: P]: P[Expr.GetArray] = P(ident ~ "[" ~/ number ~ "]").map((x) => Expr.GetArray(x._1, x._2.value))

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
        }
    }
  }

  def IfOp[_: P]: P[Expr.If] = P("if" ~ condition ~ block ~ elseOp.? ) .map{
    case (cond, ex_true, Some(ex_else)) => Expr.If(cond, ex_true, ex_else);
    case (cond, ex_true, None) => Expr.If(cond, ex_true, Expr.Block(List()));
  }

  def condition[_: P]: P[Expr] = P("(" ~/ ( negate | condOp | conditionBin | constant | condition ) ~ ")")
  def negate[_: P]: P[Expr.Not] = P( "!" ~/ condition).map(Expr.Not)
  def conditionBin[_: P]: P[Expr] = P( prefixExpr ~ StringIn("==", "!=", ">", "<", "<=", ">=").! ~/ prefixExpr).map{
    case (left, operator, right) => operator match {
      case "==" => Expr.Equals(left, right)
      case "!=" => Expr.Not(Expr.Equals(left, right))
      case "<" => Expr.LessThan(left, right)
      case ">" => Expr.MoreThan(left, right)
      case "<=" => Expr.Not(Expr.MoreThan(left, right))
      case ">=" => Expr.Not(Expr.LessThan(left, right))
    }
  }

  def condOp[_: P]: P[Expr] = P(condition ~ ("&&" | "||").! ~/ condition ~ (("&&" | "||") ~/ condition).rep).map{
    case (first, "&&", second, rest) => Expr.And(List(first, second) ::: rest.toList)
    case (first, "||", second, rest) => Expr.Or(List(first, second) ::: rest.toList)
  }

  def elseOp[_: P] = P("else" ~/ block)

  def whileLoop[_: P]: P[Expr.While] = P("while" ~/ condition ~ block).map((input)=>Expr.While(input._1, input._2))

  def str[_: P]: P[Expr] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(Expr.Str)
  def ident[_: P]: P[Expr.Ident] = P(CharIn("a-zA-Z_") ~~ CharsWhileIn("a-zA-Z0-9_", 0)).!.map((input) => {
    //checkForReservedKeyword(Expr.Ident(input))
    Expr.Ident(input)
  })
  def number[_: P]: P[Expr.Num] = P( "-".!.? ~~ CharsWhileIn("0-9", 1)).!.map(x => Expr.Num(Integer.parseInt(x)))
  def constant[_: P]: P[Expr] = P( trueC | falseC )
  def trueC[_: P]: P[Expr.True] = P("true").map(_ => Expr.True())
  def falseC[_: P]: P[Expr.False] = P("false").map(_ => Expr.False())

  def print[_: P]: P[Expr.Print] = P("print" ~/ "(" ~/ (ident | number) ~ ")").map(Expr.Print)

  class ParseException(s: String) extends RuntimeException(s)
  val reservedKeywords = List("def", "if", "while")
  def checkForReservedKeyword(input: Expr.Ident): Unit ={
    if(reservedKeywords.contains(input.name)) throw new ParseException(s"${input.name} is a reserved keyword");
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
    val codetxt = source.getLines.mkString
    source.close()
    codetxt
  }
}

object ToAssembly {
  val defaultReg = List("rax", "rdi", "rsi", "rdx", "rcx", "r8", "r9", "r10", "r11")
  def convertMain(input: Expr): String = {
    var converted =
      """ global main
        | extern printf
        | extern calloc
        | section .text
        |main:
        | sub rsp, 256
        |""".stripMargin;
    converted += convert(input, defaultReg, Map() );
    /*
    converted +=
        """mov rdi, format_num
        |mov rsi, rax
        |xor rax, rax
        |call printf
        |""".stripMargin
     */
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
      case Expr.Block(lines) => convertBlock(lines, reg, env);
      case Expr.DefineArray(size) => {
        s"mov rdi, ${size}\n" + s"mov rsi, 8\n" + "call calloc\n"
      }
      case Expr.GetArray(name, index) => {
       convert(name, reg, env) + s"mov ${reg.tail.head}, ${reg.head}\n" + s"mov ${reg.head}, [${reg.tail.head}+${index*8}]\n"
      }
      case Expr.Nothing() => "";
      case _ => throw new Exception ("not interpreted yet :(");
    }
  }
  private def convertBlock(lines: List[Expr], reg: List[String], env: Map[String, Int]): String = {
    if(lines.isEmpty) return "";
    var newenv = env;
    val defstring: String = lines.head match {
      case Expr.SetVal(Expr.Ident(name), value) => convert(value, reg, env) + setval(name, env);
      case Expr.DefVal( Expr.Ident(name)) => newenv = newVar(name, newenv); ""
      case Expr.Print(toPrint) => printInterp(toPrint, env);
      case Expr.If(condition, ifTrue, ifFalse) => {
        val trueLabel = s"if_${ifCounter}_true"
        val falseLabel = s"if_${ifCounter}_false"
        val endLabel = s"if_${ifCounter}_end"
        val ret = convertCondition(condition, reg, env, orMode = false, trueLabel, falseLabel) + s"${trueLabel}:\n" + convert(ifTrue, reg, env) +
          s"jmp ${endLabel}\n" + s"${falseLabel}:\n" + convert(ifFalse, reg, env) + s"${endLabel}:\n"
        ifCounter += 1;
        ret
      }
      case Expr.While(condition, execute) => {
        val startLabel = s"while_${ifCounter}_start"
        val trueLabel = s"while_${ifCounter}_true"
        val endLabel = s"while_${ifCounter}_end"
        val ret = s"${startLabel}:\n" + convertCondition(condition, reg, env, orMode = false, trueLabel, endLabel) + s"${trueLabel}:\n" + convert(execute, reg, env) +
          s"jmp ${startLabel}\n" + s"${endLabel}:\n"
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
  private def convertCondition(input: Expr, reg: List[String], env: Map[String, Int], orMode: Boolean, trueLabel: String, falseLabel: String): String = {
    def compare(left: Expr, right: Expr): String = binOpTemplate(left, right, "cmp", reg, env)
    val newtrueLabel = s"cond_${subconditionCounter}_true"
    val newfalseLabel = s"cond_${subconditionCounter}_false"
    val ret = input match {
      case Expr.True() => if(orMode) s"jmp ${trueLabel}\n" else ""
      case Expr.False() => if(!orMode) s"jmp ${falseLabel}\n" else ""
      case Expr.Equals(left, right) => {
        compare(left, right) + ( if(orMode) s"je ${trueLabel}\n" else s"jne ${falseLabel}\n" )
      }
      case Expr.LessThan(left, right) => {
        compare(left, right) + ( if(orMode) s"jl ${trueLabel}\n" else s"jge ${falseLabel}\n" )
      }
      case Expr.MoreThan(left, right) => {
        compare(left, right) + ( if(orMode) s"jg ${trueLabel}\n" else s"jle ${falseLabel}\n" )
      }
      case Expr.Not(cond) => {
        subconditionCounter += 1
        convertCondition(cond, reg, env, orMode = orMode, newtrueLabel, newfalseLabel) +
        s"${newtrueLabel}:\n" + s"jmp ${falseLabel}\n" + s"${newfalseLabel}:\n" + s"jmp ${trueLabel}\n"
      }
      case Expr.And(list) => {
        subconditionCounter += 1;
        list.foldLeft("")((acc, subcond) => acc + convertCondition(subcond, reg, env, orMode = false, newtrueLabel, newfalseLabel)) +
          s"${newtrueLabel}:\n" + s"jmp ${trueLabel}\n" + s"${newfalseLabel}:\n" + s"jmp ${falseLabel}\n"
      }
      case Expr.Or(list) => {
        subconditionCounter += 1;
        list.foldLeft("")((acc, subcond) => acc + convertCondition(subcond, reg, env, orMode = true, newtrueLabel, newfalseLabel)) +
        s"${newfalseLabel}:\n" + s"jmp ${falseLabel}\n" + s"${newtrueLabel}:\n" + s"jmp ${trueLabel}\n" 
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
  type Env = Map[String, Int]
  case class Variable(stackPointer: Int, varType: String)
}

