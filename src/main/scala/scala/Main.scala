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
  case class NumFloat(value: Float) extends Expr
  case class Character(char: Char)

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
  case class DefVal(variable: Expr, varType: Type) extends Expr
  case class Block(lines: List[Expr]) extends Expr
  case class ExtendBlock(lines: List[Expr]) extends Expr
  case class While(condition: Expr, execute: Expr.Block) extends Expr

  case class GetArray(array: Ident, index: Expr) extends Expr
  case class SetArray(array: Ident, index: Expr, value: Expr) extends Expr
  case class DefineArray(size: Int, defaultValues: List[Expr]) extends Expr
  case class ConcatArray(left: Expr, right: Expr) extends Expr
  case class ArraySize(array: Ident) extends Expr

  //case class StackVar(offset: Int) extends Expr
  case class Func(name: String, argNames: List[InputVar], retType: Type, body: Expr.Block) extends Expr
  case class CallF(name: String, args: List[Expr]) extends Expr
  case class Return(value: Expr) extends Expr

  case class TopLevel(functions: List[Func]) extends Expr
  case class Nothing() extends Expr
}

sealed trait Type
object Type {
  case class Undefined() extends Type
  case class Num() extends Type
  case class NumFloat() extends Type
  case class Character() extends Type
  case class Array(size: Int, elemType: Type) extends Type
}
case class InputVar(name: String, varType: Type)

object Parser {

  def topLevel[_: P]: P[Expr.TopLevel] = P(function.rep).map(x=>Expr.TopLevel(x.toList))
  def function[_: P]: P[Expr.Func] = P("def " ~/ ident ~ "(" ~/ functionArgs ~ ")" ~/ typeDef.? ~ block).map{
    case (name, args, retType, body) => {
      val ret = retType match {
        case Some(typ) => typ
        case None => Type.Undefined()
      }
      Expr.Func(name.name, args, ret, body)
    }
  }
  def functionArgs[_: P]: P[List[InputVar]] = P( (ident ~ typeDef).rep(sep = ",")).map(x=>x.map(y=>InputVar(y._1.name, y._2)).toList)
  //def parseType[_: P] : P[Type] = P(ident ~ "(" ~/ ")")
  def block[_: P] = P("{" ~/ line.rep(0) ~ "}").map ( lines => lines.foldLeft(List(): List[Expr])( (acc, el) => el match {
    case Expr.ExtendBlock(sub) => acc ::: sub;
    case _ => acc :+ el;
  } )).map(x=>Expr.Block(x))
  def line[_: P]: P[Expr] = P(expr ~/ ";")
  def expr[_: P]: P[Expr] = P( arrayDef | arrayDefDefault | defAndSetVal | defVal | setArray | setVal | retFunction | IfOp | whileLoop | print)

  def prefixExpr[_: P]: P[Expr] = P( parens | arrayDef | arrayDefDefault | getArray | callFunction | getArraySize | numberFloat | number | ident | constant | str)

  def defVal[_: P] = P("val " ~ ident ~ typeDef.?).map{
    case(ident, Some(varType)) => Expr.DefVal(ident, varType)
    case(ident, None) => Expr.DefVal(ident, Type.Undefined())
  }
  def setVal[_: P] = P(ident ~ "=" ~/ prefixExpr).map(x => Expr.SetVal(x._1, x._2))
  def defAndSetVal[_: P] = P( defVal ~ "=" ~ prefixExpr).map(x => Expr.ExtendBlock(List(x._1, Expr.SetVal(x._1.variable, x._2))))

  /*
  def arrayDef[_: P] = P("val " ~ ident ~ "=" ~ "Array" ~/ "[" ~ number ~ "]").map(x => {
    if(x._2.value < 0) throw new ParseException("negative array size");
    Expr.ExtendBlock(List(
      Expr.DefVal(x._1),
      Expr.SetVal(x._1, Expr.DefineArray(x._2.value))
    ));
  })
   */
  def arrayDef[_: P]: P[Expr.DefineArray] = P("array" ~ "[" ~/ number ~ "]").map(x=> Expr.DefineArray(x.value, List()))
  def arrayDefDefault[_: P]: P[Expr.DefineArray] = P("array" ~ "(" ~/ prefixExpr.rep(sep = ",") ~ ")").map(x=> Expr.DefineArray(x.size, x.toList))

  def getArray[_: P]: P[Expr.GetArray] = P(ident ~ "[" ~/ prefixExpr ~ "]").map((x) => Expr.GetArray(x._1, x._2))
  def setArray[_: P]: P[Expr.SetArray] = P(ident ~ "[" ~/ prefixExpr ~ "]" ~/ "=" ~ prefixExpr).map((x) => Expr.SetArray(x._1, x._2, x._3))
  def getArraySize[_: P]: P[Expr.ArraySize] = P(ident ~ ".size").map((x) => Expr.ArraySize(x))

  def retFunction[_: P]: P[Expr.Return] = P("return" ~/ prefixExpr).map(Expr.Return)

  def typeDef[_: P]: P[Type] = P(":" ~/ StringIn("int", "char").!).map{
    case "int" => Type.Num();
    case "char" => Type.Character();
  }

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

  def callFunction[_: P]: P[Expr.CallF] = P( ident ~ "(" ~/ prefixExpr.rep(sep = ",") ~ ")" ).map{
    case (name, args) => Expr.CallF(name.name, args.toList);
  }

  def binOp[_: P] = P( prefixExpr ~ ( StringIn("+", "-", "*", "/", "++").! ~/ prefixExpr).rep(1)).map(list => parseBinOpList(list._1, list._2.toList))

  def parseBinOpList(initial: Expr, rest: List[(String, Expr)]): Expr = {
    rest.foldLeft(initial) {
      case (left, (operator, right)) => operator match {
        case "+" => Expr.Plus(left, right)
        case "-" => Expr.Minus(left, right)
        case "*" => Expr.Mult(left, right)
        case "/" => Expr.Div(left, right)
        case "++" => Expr.ConcatArray(left, right)
      }
    }
  }

  def IfOp[_: P]: P[Expr.If] = P("if" ~/ condition ~/ block ~/ elseOp.? ) .map{
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
  def numberFloat[_: P]: P[Expr.NumFloat] = P( "-".? ~~ CharsWhileIn("0-9", 1) ~~ "." ~~ CharsWhileIn("0-9", 1)).!.map(x => Expr.NumFloat(x.toFloat))
  //def char[_: P]: P[Expr.Num] = P( "-".!.? ~~ CharsWhileIn("0-9", 1)).!.map(x => Expr.Num(Integer.parseInt(x)))
  def constant[_: P]: P[Expr] = P( trueC | falseC )
  def trueC[_: P]: P[Expr.True] = P("true").map(_ => Expr.True())
  def falseC[_: P]: P[Expr.False] = P("false").map(_ => Expr.False())

  def print[_: P]: P[Expr.Print] = P("print" ~/ "(" ~/ ( NoCut(binOp) | prefixExpr ) ~ ")").map(Expr.Print)

  class ParseException(s: String) extends RuntimeException(s)
  val reservedKeywords = List("def", "val", "if", "while", "true", "false", "array")
  def checkForReservedKeyword(input: Expr.Ident): Unit ={
    if(reservedKeywords.contains(input.name)) throw new ParseException(s"${input.name} is a reserved keyword");
  }

  def parseInput(input: String): Expr = {
    val parsed = fastparse.parse(input, topLevel(_));
    parsed match {
      case Parsed.Success(expr, n) => expr;
      case t: Parsed.Failure => {println(t.trace().longAggregateMsg); throw new ParseException("parsing fail");}
      case _ => throw new ParseException("parsing fail")
    }
  }
}

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

object ToAssembly {
  val defaultReg = List("rax", "rdi", "rsi", "rdx", "rcx", "r8", "r9", "r10", "r11")
  def convertMain(input: Expr): String = {
    /*
    var converted =
      """ global main
        | extern printf
        | extern calloc
        | extern free
        | section .text
        |main:
        | sub rsp, 256
        |""".stripMargin;
    converted += convert(input, defaultReg, Map() )._1;
    converted += "add rsp, 256\n"
    converted += "  mov rax, 0\n  ret\n"
    converted += "format_num:\n        db  \"%d\", 10, 0"
    converted
     */
    var converted =
      """ global main
        | extern printf
        | extern calloc
        | extern free
        | section .text
        |""".stripMargin;
    input match { case x: Expr.TopLevel => {
      declareFunctions(x);
      converted += defineFunctions(x);
    }}

    //converted += convert(input, defaultReg, Map() )._1;
    converted += "format_num:\n        db  \"%d\", 10, 0\n"
    converted += "format_float:\n        db  \"%f\", 10, 0\n"
    //converted.split("\n").zipWithIndex.foldLeft("")((acc, v)=> acc +s"\nline${v._2}:\n"+ v._1)
    converted
  }
  var lineNr = 0;
  var ifCounter = 0;
  var subconditionCounter: Int = 0;
  var functions: List[FunctionInfo] = List();
  var functionScope: FunctionInfo = FunctionInfo("main", List(), Type.Num());
  private def convert(input: Expr, reg: List[String], env: Env): (String, Type) = {
    input match {
      case Expr.Num(value) => (s"mov ${reg.head}, 0${value}d\n", Type.Num())
      case Expr.NumFloat(value) => {
        (s"mov ${reg.head}, __float64__(${value.toString})\n", Type.NumFloat())
      }
      case Expr.Plus(left, right) => {
        (convert(left, reg, env), convert(right, reg.tail, env)) match {
          case ((codeLeft, Type.Num()), (codeRight, Type.Num())) =>
            (codeLeft + codeRight + s"add ${reg.head}, ${reg.tail.head}\n", Type.Num());
          case ((codeLeft, Type.NumFloat()), (codeRight, Type.NumFloat())) =>{
            val ret = codeLeft + s"movq xmm0, ${reg.head}" + codeRight + s"addss ${reg.head}, ${reg.tail.head}\n"
            (ret, Type.NumFloat());
          }

          case ((codeLeft, typeLeft), (codeRight, typeRight)) => throw new Exception(s"can't add operands of types ${typeLeft} and ${typeRight}");
        }
      }
      /*
      case Expr.ConcatArray(left, right) => (convert(left, reg, env), convert(right, reg.tail, env)) match {
        case ((codeLeft, Type.Array(sizeLeft, elemTypeLeft)), (codeRight, Type.Array(sizeRight, elemTypeRight))) => {
          if(elemTypeLeft != elemTypeRight) throw new Exception(s"can't add arrays of different types");
          val size = sizeLeft+ sizeRight;
          var ret = codeLeft + "push rax\n" + codeRight + "push rax\n";
          val leftArr = "[rsp-8]"; val rightArr = "[rsp-16]"
          //TODO add runtime garbage collection
          //s"mov rdi, [rbp-${entry._2.pointer}]\n" + "call free\n";
          ret += s"mov rdi, ${size}\n" + s"mov rsi, 8\n" + "call calloc\n" + "push rax\n";
          ret += Expr.While()
          ret += "pop rax\n"
          (ret, Type.Array(size, elemTypeLeft))
        }
      }
       */
      case Expr.Minus(left, right) => (binOpTemplate(left, right, "sub", reg, env), Type.Num())
      case Expr.Mult(left, right) => (mulTemplate(left, right, "imul", reg, env), Type.Num())
      case Expr.Ident(name) => {
        val look = lookup(name, env)
        (s"mov ${reg.head}, ${look._1}\n", look._2.varType)
      }
      //case Expr.Div(left, right) => mulTemplate(left, right, "idiv", reg)
      case Expr.Block(lines) => convertBlock(lines, reg, env);
      case Expr.DefineArray(size, defaultValues) => defineArray(size, defaultValues, env)
      case Expr.GetArray(name, index) => getArray(name, index, reg, env);
      case Expr.ArraySize(name) => getArraySize(name, reg, env);
      case Expr.CallF(name, args) => functions.find(x=>x.name == name) match {
        case Some(FunctionInfo(p, argTypes, retType)) => {
          if(argTypes.length != args.length) throw new Exception (s"wrong number of arguments: expected ${argTypes.length}, got ${args.length}")
          val usedReg = defaultReg.filter(x => !reg.contains(x));
          var ret = usedReg.map(x=>s"push $x\n").mkString
          ret += args.zipWithIndex.map{case (arg, index) => {
            val converted = convert(arg, reg, env)
            if(converted._2 != argTypes(index)) throw new Exception (s"wrong argument type: expected ${argTypes(index)}, got ${converted._2}")
            converted._1 + s"push ${reg.head}\n"
          }}.mkString
          ret += args.zipWithIndex.reverse.map{case (arg, index) => s"pop ${functionCallReg(index)}\n"}.mkString
          ret += s"call $name\n"
          ret += s"mov ${reg.head}, rax\n"
          ret += usedReg.reverse.map(x=>s"pop $x\n").mkString
          (ret, retType)
        }
        case None => throw new Exception (s"function of name $name undefined");
      }
      case Expr.Nothing() => ("", Type.Undefined());
      case _ => throw new Exception ("not interpreted yet :(");
    }
  }
  private def convertBlock(lines: List[Expr], reg: List[String], env: Env): (String, Type) = {
    if(lines.isEmpty) return ("", Type.Undefined());
    var newenv = env;
    var defstring: String = lines.head match {
      case Expr.SetVal(Expr.Ident(name), value) => {
        val converted = convert(value, reg, env);
        val modified = setval(name, converted._2, env)
        newenv = modified._2
        converted._1 + modified._1
      };
      case Expr.SetArray(Expr.Ident(name), index, value) => {
        val converted = convert(value, reg, env);
        val arr = setArray(name, index, converted._2, env)
        newenv = arr._2;
        converted._1 + arr._1
      };
      case Expr.DefVal(Expr.Ident(name), varType) => newenv = newVar(name, varType, newenv); ""
      case Expr.Print(toPrint) => printInterp(toPrint, env);
      case Expr.If(condition, ifTrue, ifFalse) => {
        val trueLabel = s"if_${ifCounter}_true"
        val falseLabel = s"if_${ifCounter}_false"
        val endLabel = s"if_${ifCounter}_end"
        ifCounter += 1;
        val ret = convertCondition(condition, reg, env, orMode = false, trueLabel, falseLabel) + s"${trueLabel}:\n" + convert(ifTrue, reg, env)._1 +
          s"jmp ${endLabel}\n" + s"${falseLabel}:\n" + convert(ifFalse, reg, env)._1 + s"${endLabel}:\n"
        ret
      }
      case Expr.While(condition, execute) => {
        val startLabel = s"while_${ifCounter}_start"
        val trueLabel = s"while_${ifCounter}_true"
        val endLabel = s"while_${ifCounter}_end"
        val ret = s"${startLabel}:\n" + convertCondition(condition, reg, env, orMode = false, trueLabel, endLabel) + s"${trueLabel}:\n" + convert(execute, reg, env)._1 +
          s"jmp ${startLabel}\n" + s"${endLabel}:\n"
        ifCounter += 1;
        ret
      }
      case Expr.Return(value) => {
        val converted = convert(value, defaultReg, env)
        if(functionScope.retType != converted._2) throw new Exception(s"Wrong return argument: function ${functionScope.name} expects ${functionScope.retType}, got ${converted._2}")
        converted._1 + "leave\nret\n"
      }
      case _ => throw new Exception(lines.head.toString + " should not be in block lines")
    }
    if(lines.tail.nonEmpty) {
      lineNr+=1;
      defstring += convert(Expr.Block(lines.tail), defaultReg, newenv)._1
    }
    //else defstring += freeMemory(env)
    (defstring, Type.Undefined());
  }
  private def convertCondition(input: Expr, reg: List[String], env: Env, orMode: Boolean, trueLabel: String, falseLabel: String): String = {
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
  private def declareFunctions(input: Expr.TopLevel): Unit = {
    functions = input.functions.map(x=> FunctionInfo(x.name, x.argNames.map(y=>y.varType), x.retType))
  }
  val functionCallReg = List( "rdi", "rsi", "rdx", "rcx", "r8", "r9")
  private def defineFunctions(input: Expr.TopLevel): String = {
    input.functions.map(function => {
      val info = functions.find(x=>x.name == function.name).get;
      functionScope = info;
      var ret = s"${function.name}:\n" +
      """ push rbp
       | mov rbp, rsp
       | sub rsp, 256
       |""".stripMargin;
      var env: Env = Map()
      var regArgs = functionCallReg;
      ret += function.argNames.map(arg => {
        env = newVar(arg.name, arg.varType, env)
        val moveVar = s"mov qword ${lookup(arg.name, env)._1}, ${regArgs.head}\n"
        regArgs = regArgs.tail;
        moveVar
      }).mkString
      ret += convert(function.body, defaultReg, env)._1
      ret
    }).mkString
  }
  //TODO remove type assingmed after fact(causes issues when type is unknown in compile time)
  def setval(name: String, raxType: Type, env: Env): (String, Env) = {
    val look = lookup(name, env);
    var newenv = env;
    if(look._2.varType != raxType) {
      if(look._2.varType == Type.Undefined()) newenv = env.map(x=>if(x._1==name) (x._1, Variable(x._2.pointer, raxType)) else x)
      else throw new Exception(s"trying to set variable of type ${look._2.varType} to $raxType")
    }
    (s"mov qword ${look._1}, rax\n", newenv)
  }
  //TODO add runtime index checking
  def setArray(name: String, index: Expr, raxType: Type, env: Env): (String, Env) = (lookup(name, env), convert(index, defaultReg, env)) match {
    case ((varLoc, Variable(loc, Type.Array(size, elemType))), (indexCode, indexType)) => {
      var newenv = env;
      if(elemType != raxType) {
        if(elemType == Type.Undefined()) newenv = env.map(x=>if(x._1==name) (x._1, Variable(x._2.pointer, Type.Array(size, raxType))) else x)
        else throw new Exception(s"trying to set array element of type ${elemType} to $raxType")
      }
      if(indexType != Type.Num()) throw new Exception(s"wrong index for array, got $indexType")
      ("push rax\n" + indexCode + s"mov rdi, ${varLoc}\n" + "pop rsi\n" + s"mov [rdi+8+rax*8], rsi\n", newenv)
    }
  }
  def getArray(name: Expr.Ident, index: Expr, reg: List[String], env: Env): (String, Type) = (convert(name, reg, env), convert(index, reg, env)) match {
    case ((code, Type.Array(size, arrType)), (indexCode, indexType)) => {
      if(indexType != Type.Num()) throw new Exception(s"wrong index for array, got $indexType")
      (code + s"push ${reg.head}\n" + indexCode + s"mov ${reg.tail.head}, ${reg.head}\n" + s"pop ${reg.head}\n" + s"mov ${reg.head}, [${reg.head}+8+${reg.tail.head}*8]\n", arrType)
    }
    case ((code, varType), l) => throw new Exception(s"trying to access variable ${name.name} as an array, has type $varType")
  }
  def getArraySize(name: Expr.Ident, reg: List[String], env: Env): (String, Type) = convert(name, reg, env) match {
    case (code, Type.Array(size, arrType)) => {
      (code + getArrayDirect(reg.head, 0, reg), Type.Num())
    }
    case (code, varType) => throw new Exception(s"trying to access variable ${name.name} as an array, has type $varType")
  }
  def setArrayDirect(code: String, index: Int): String = {
      s"mov rdi, ${code}\n" + s"mov [rdi+${index*8}], rax\n"
  }
  def getArrayDirect(code: String, index: Int, reg: List[String]): String = {
    s"mov ${reg.tail.head}, ${code}\n" + s"mov ${reg.head}, [${reg.tail.head}+${index*8}]\n"
  }
  def defineArray(size: Int, defaultValues: List[Expr], env: Env): (String, Type) = {
    var ret = s"mov rdi, ${size+1}\n" + s"mov rsi, 8\n" + "call calloc\n" + "push rax\n" + "mov r9, rax\n";
    var elemType: Type = Type.Undefined();
    ret += defaultValues.zipWithIndex.map{case (entry, index) => {
      val converted = convert(entry, defaultReg, env)
      if(elemType == Type.Undefined()) elemType = converted._2;
      else if(converted._2 != elemType) throw new Exception(s"array elements are of different types")
      converted._1 + setArrayDirect("[rsp]", index+1);
    }}.mkString;
    ret += s"mov rax, 0${size}d\n" + setArrayDirect("[rsp]", 0)
    ret += "pop rax\n"
    (ret, Type.Array(size, elemType))
  }

  def lookup(tofind: String, env: Env): (String, Variable) = {
    val ret = lookupOffset(tofind, env)
    (s"[rbp-${ret.pointer}]", ret)
  }
  def lookupOffset(tofind: String, env: Env): Variable = env.get(tofind) match {
    case Some(v) => v
    case None => throw new Exception(s"variable \"${tofind}\" undefined")
  }
  def newVar(name: String, varType: Type, env: Env) : Env = {
    if(env.contains(name)) throw new Exception(s"variable \"${name}\" already defined")
    val newOffset: Int = if(env.isEmpty) 0 else env.values.toList.map(x=>x.pointer).max
    env + (name -> Variable(newOffset + 8, varType))
  }
  def mulTemplate(left: Expr, right: Expr, command: String, reg: List[String], env: Env): String = {
    val leftout = convert(left, reg, env)._1;
    val rightout = convert(right, reg.tail, env)._1;
    leftout + rightout + s"${command} ${reg.tail.head}\n";
  }
  def binOpTemplate(left: Expr, right: Expr, command: String, reg: List[String], env: Env): String = {
    val leftout = convert(left, reg, env)._1;
    val rightout = convert(right, reg.tail, env)._1;
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
  def printInterp(toPrint: Expr, env: Env): String = {
    val converted = convert(toPrint, defaultReg, env)
    converted._2 match {
      case Type.Num() => converted._1 + printTemplate("format_num");
      case Type.NumFloat() => converted._1 + "movq xmm0, rax\n" + "mov rdi, format_float\n" + "mov rax, 1\n" + "call printf\n"
      case _ => throw new Exception(s"input of type ${converted._2} not recognized in print")
    }
  }
  def freeMemory(env: Env): String = env.foldLeft("")((acc, entry) => entry._2.varType match {
    case Type.Array(size, arrType) => acc + s"mov rdi, [rbp-${entry._2.pointer}]\n" + "call free\n";
    case _ => acc
  })

  type Env = Map[String, Variable]
  case class FunctionInfo(name: String, args: List[Type], retType: Type)
  case class Variable(pointer: Int, varType: Type)
}


