package posharp

import fastparse.JavaWhitespace.*
import fastparse.*
import Expr.GetProperty
import jdk.jshell.spi.ExecutionControl.NotImplementedException

import scala.compat.Platform.EOL
import scala.util.{Failure, Success, Try}

object Parser {
  //TODO fix issue when spacing at start of file
  def topLevel[$: P]: P[Expr.TopLevel] = P(StringIn(" ").? ~ (function | interfaceDef | enumDef | imports).rep(1)).map(x => {
    var func: List[Expr.Func] = List()
    var intf: List[Expr.DefineInterface] = List()
    var enumm: List[Expr.DefineEnum] = List()
    var imports: List[Expr.Import] = List()
    x.foreach {
      case y@Expr.Func(a, b, c, d, e) => func = func :+ y
      case y@Expr.DefineInterface(a, b, c, d) => intf = intf :+ y
      case y@Expr.DefineEnum(a, b) => enumm = enumm :+ y
      case y@Expr.Import(a, b) => imports = imports :+ y
    }
    Expr.TopLevel(func, intf, enumm, imports)
  })

  def function[$: P]: P[Expr.Func] = P("def " ~/ mod_ident ~ templateTypes.? ~ "(" ~/ functionArgs ~ ")" ~/ typeDef.? ~ block).map {
    case (name, templates, args, retType, body) => {
      val ret = retType match {
        case Some(typ) => typ
        case None => Type.Undefined()
      }
      Expr.Func(name.name, args, ret, body, templates.getOrElse(List()).asInstanceOf[List[Type.T]])
    }
  }
  def function_intf[$: P]: P[Expr.Func] = P("def " ~/ ident ~ templateTypes.? ~ "(" ~/ functionArgs ~ ")" ~/ typeDef.? ~ block).map {
    case (name, templates, args, retType, body) => {
      val ret = retType match {
        case Some(typ) => typ
        case None => Type.Undefined()
      }
      Expr.Func(name.name, args, ret, body, templates.getOrElse(List()).asInstanceOf[List[Type.T]])
    }
  }

  def imports[$: P]: P[Expr.Import] = P(importPartial | importModule)
  def importPartial[$: P]: P[Expr.Import] = P("import " ~ ident ~ "from" ~/ fileName ~ ";").map(x=> Expr.Import(x._1.name, x._2.s))
  def importModule[$: P]: P[Expr.Import] = P("import " ~/ fileName ~ ";").map(x=> Expr.Import("__module__", x.s))

  /*
  def interfaceDef[$: P]: P[Expr.DefineInterface] = P("interface " ~ ident ~/ "{" ~ (ident ~ typeDef).rep(sep = ",") ~ "}").map(props=>
    Expr.DefineInterface(props._1.name, props._2.toList.map(x=>InputVar(x._1.name, x._2)))
  )
   */
  def interfaceDef[$: P]: P[Expr.DefineInterface] = P("object " ~/ mod_ident ~ templateTypes.? ~ "{" ~/ objValue ~ function_intf.rep ~ "}").map(props =>
    Expr.DefineInterface(props._1.name, props._3.map(x => InputVar(x.name, x.varType)), props._4.toList, props._2.getOrElse(List()).asInstanceOf[List[Type.T]]))

  def objValue[$: P]: P[List[ObjVal]] = P(ident ~ typeDef ~ ("=" ~ prefixExpr).? ~ ";").rep.map(x => x.map {
    case (id, valtype, Some(value)) => ObjVal(id.name, valtype, value)
    case (id, valtype, None) => ObjVal(id.name, valtype, Type.defaultValue(valtype))
  }.toList)

  def enumDef[$: P]: P[Expr.DefineEnum] = P("enum " ~ mod_ident ~/ "{" ~ ident.rep(sep = ",") ~ "}").map(props =>
    Expr.DefineEnum(props._1.name, props._2.toList.map(x => x.name))
  )

  def functionArgs[$: P]: P[List[InputVar]] = P((ident ~ typeDef).rep(sep = ",")).map(x => x.map(y => InputVar(y._1.name, y._2)).toList)

  //def parseType[$: P] : P[Type] = P(ident ~ "(" ~/ ")")
  def block[$: P] = P("{" ~/ line.rep(0) ~ "}").map(lines => lines.foldLeft(List(): List[Expr])((acc, el) => el match {
    case Expr.ExtendBlock(sub) => acc ::: sub;
    case _ => acc :+ el;
  })).map(x => Expr.Block(x))

  def line[$: P]: P[Expr] = P(expr ~/ ";")

  def expr[$: P]: P[Expr] = P(arrayDef | arrayDefDefault | defAndSetVal | defVal | NoCut(setVar) | callFuncInLine | retFunction | IfOp | whileLoop | forLoop | print | free | callFunction | throwException)

  def prefixExpr[$: P]: P[Expr] = P( NoCut(callFunction) | defineLambda | NoCut(convert) | NoCut(parens) | NoCut(condition) | arrayDef | arrayDefDefault | instanceInterface | accessVar | NoCut(getArraySize) |
    numberFloat | number | ident | constant | str | char | trueC | falseC)

  def defVal[$: P]: P[Expr.DefVal] = P("val " ~/ ident ~ typeDef.?).map {
    case (ident, Some(varType)) => Expr.DefVal(ident.name, varType)
    case (ident, None) => Expr.DefVal(ident.name, Type.Undefined())
  }

  def accessVar[$: P]: P[Expr] = P(ident ~ ((".".! ~ ident ~ "(".! ~ prefixExpr.rep(sep = ",") ~ ")") | (".".! ~ ident) | ("[".! ~/ prefixExpr ~ "]")).rep)
    .map { case (start, acs) =>
    acs.foldLeft(start: Expr)((acc, v) => v match {
      case (".", Expr.Ident(ident)) => GetProperty(acc, ident)
      case ("[", index: Expr) => Expr.GetArray(acc, index)
      //TODO template functions not handled
      case (".", Expr.Ident(ident), "(", args: List[Expr]) => {
        //val callf_prefix = if(prefix.nonEmpty) prefix else file_name
        //callf_prefix+"_"+
        Expr.CallObjFunc(acc, Expr.CallF(ident, args, List()))
      }//throw new NotImplementedException("")
      case x => throw new ParseException(s"bad var access: $x");
    })
  }

  def callFuncInLine[$: P]: P[Expr] = P(accessVar).filter {
    case _: Expr.CallObjFunc => true
    case x => false
  }

  def setVar[$: P]: P[Expr] = P(accessVar ~ StringIn("+=", "-=", "*=", "/=", "=").! ~/ prefixExpr ~ &(";")).map {
    case (variable, op, value) => {
      val opType = op match {
        case "=" => value
        case "+=" => Expr.Plus(variable, value)
        case "-=" => Expr.Minus(variable, value)
        case "*=" => Expr.Mult(variable, value)
        case "/=" => Expr.Div(variable, value)
      }
      (variable, opType) match {
        case (id: Expr.Ident, value) => Expr.SetVal(id, value)
        case (Expr.GetProperty(expr, ident), value) => Expr.SetInterfaceProp(expr, ident, value)
        case (Expr.GetArray(arr, index), value) => Expr.SetArray(arr, index, value)
        case x => throw new ParseException(s"bad var set: $x");
      }

    }
  }

  def defAndSetVal[$: P] = P(defVal ~ "=" ~/ prefixExpr).map(x => Expr.DefValWithValue(x._1.variable, x._1.varType, x._2))

  def defineLambda[$: P]: P[Expr.Lambda] = P("lambda" ~/ "(" ~ (ident ~ typeDef).rep(sep=",") ~ ")" ~ typeDef ~ "=>" ~/ (block | prefixExpr) ).map{ case (args, ret, body) => {
    val argsf = args.map(x=>InputVar(x._1.name, x._2)).toList
    body match {
      case b@Expr.Block(_) => Expr.Lambda(argsf, ret, b)
      case b => Expr.Lambda(argsf, ret, Expr.Block(List(Expr.Return(Some(b)))))
    }
  }}

  def arrayDef[$: P]: P[Expr.DefineArray] = P("array" ~ "[" ~/ typeBase ~ "]" ~ "[" ~/ prefixExpr ~ "]").map(x => Expr.DefineArray(x._2, x._1, List()))
  def arrayDefDefault[$: P]: P[Expr.DefineArray] = P("array" ~ "(" ~/ prefixExpr.rep(sep = ",") ~ ")").map(x => Expr.DefineArray(Expr.Num(x.size), Type.Undefined(), x.toList))

  /*

  def getArray[$: P]: P[Expr.GetArray] = P(returnsArray ~ "[" ~/ prefixExpr ~ "]").map((x) => Expr.GetArray(x._1, x._2))
  def setArray[$: P]: P[Expr.SetArray] = P(returnsArray ~ "[" ~/ prefixExpr ~ "]" ~/ "=" ~ prefixExpr).map((x) => Expr.SetArray(x._1, x._2, x._3))
   */
  def returnsArray[$: P]: P[Expr] = P(ident)

  def getArraySize[$: P]: P[Expr.ArraySize] = P(returnsArray ~~ ".size").map((x) => Expr.ArraySize(x))


  def instanceInterface[$: P]: P[Expr.InstantiateInterface] = P("new " ~/ mod_ident ~ templateTypes.? ~ "(" ~/ prefixExpr.rep(sep = ",") ~ ")")
    .map(x => Expr.InstantiateInterface(x._1.name, x._3.toList, x._2.getOrElse(List())))

  def typeDef[$: P]: P[Type] = P(":" ~/ (typeBase | typeArray | typeFunc | typeUser))
  def typeDefNoCol[$: P]: P[Type] = P(typeBase | typeArray | typeFunc | typeUser)

  def typeArray[$: P]: P[Type] = P("array" ~/ "[" ~ typeDefNoCol ~ "]").map(x => Type.Array(x))
  def typeFunc[$: P]: P[Type] = P("func" ~/ "[" ~ "(" ~ typeDefNoCol.rep(sep=",") ~ ")" ~/ "=>" ~ typeDefNoCol ~ "]").map(x => Type.Function(x._1.toList, x._2))

  def typeUser[$: P]: P[Type] = P(mod_ident ~ templateTypes.?).map(x => Type.UserType(x._1.name, x._2.getOrElse(List())))

  def typeBase[$: P]: P[Type] = P((StringIn("int", "char", "float", "bool", "string", "void").!) | ("T" ~ CharsWhileIn("0-9", 1)).!).map {
    case "int" => Type.Num();
    case "char" => Type.Character();
    case "float" => Type.NumFloat();
    case "bool" => Type.Bool();
    case "string" => Type.Str();
    case "void" => Type.Undefined();
    case "T1" => Type.T(1);
    case "T2" => Type.T(2);
  }

  def parens[$: P] = P("(" ~/ (binOp | prefixExpr) ~ ")")

  def convert[$: P]: P[Expr.Convert] = P("(" ~ (binOp | prefixExpr) ~ ")" ~ "." ~/ StringIn("toInt", "toFloat", "toChar").!).map {
    case (value, "toInt") => Expr.Convert(value, Type.Num())
    case (value, "toFloat") => Expr.Convert(value, Type.NumFloat())
    case (value, "toChar") => Expr.Convert(value, Type.Character())
  }

  def callFunction[$: P]: P[Expr.CallF] = P(mod_ident ~ templateTypes.? ~ "(" ~/ prefixExpr.rep(sep = ",") ~/ ")").map {
    case (name, templates, args) => Expr.CallF(name.name, args.toList, templates.getOrElse(List()));
  }//.filter((x) => !reservedKeywords.contains(x.name))

  def templateTypes[$: P]: P[List[Type]] = (P("[" ~/ typeDefNoCol.rep(min=1, sep = ",") ~/ "]")).map(x=>x.toList)

  def retFunction[$: P]: P[Expr.Return] = P("return" ~/ prefixExpr.?).map(Expr.Return)

  def binOp[$: P] = P(prefixExpr ~ (StringIn("+", "-", "*", "/", "++").! ~/ prefixExpr).rep(1)).map(list => parseBinOpList(list._1, list._2.toList))

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

  def IfOp[$: P]: P[Expr.If] = P("if" ~/ "(" ~ conditionNoParen ~ ")" ~/ block ~/ elseOp.?).map {
    case (cond, ex_true, Some(ex_else)) => Expr.If(cond, ex_true, ex_else);
    case (cond, ex_true, None) => Expr.If(cond, ex_true, Expr.Block(List()));
  }

  def condition[$: P]: P[Expr] = P(("(" ~/ conditionNoParen ~ ")") | returnsBool)

  def conditionNoParen[$: P]: P[Expr] = P(negate | condOp | conditionBin | constant | returnsBool | condition)
  def returnsBool[$: P]: P[Expr] = P(NoCut(convert) | NoCut(parens) | accessVar | callFunction | ident | constant)

  def negate[$: P]: P[Expr.Not] = P("!" ~/ condition).map(Expr.Not)

  def conditionBin[$: P]: P[Expr] = P(prefixExpr ~ StringIn("==", "!=", ">", "<", "<=", ">=").! ~/ prefixExpr).map {
    case (left, operator, right) => operator match {
      case "==" => Expr.Equals(left, right)
      case "!=" => Expr.Not(Expr.Equals(left, right))
      case "<" => Expr.LessThan(left, right)
      case ">" => Expr.MoreThan(left, right)
      case "<=" => Expr.Not(Expr.MoreThan(left, right))
      case ">=" => Expr.Not(Expr.LessThan(left, right))
    }
  }

  def condOp[$: P]: P[Expr] = P(condition ~ ("&&" | "||").! ~/ condition ~ (("&&" | "||") ~/ condition).rep).map {
    case (first, "&&", second, rest) => Expr.And(List(first, second) ::: rest.toList)
    case (first, "||", second, rest) => Expr.Or(List(first, second) ::: rest.toList)
  }

  def elseOp[$: P] = P("else" ~/ block)

  def whileLoop[$: P]: P[Expr.While] = P("while" ~/ condition ~ block).map((input) => Expr.While(input._1, input._2))

  def forLoop[$: P]: P[Expr.Block] = P("for" ~/ "(" ~ line ~ conditionNoParen ~ ";" ~ line ~ ")" ~/ block).map((input) => {
    Expr.Block(List(input._1, Expr.While(input._2, Expr.Block(input._4.lines :+ input._3))));
  })

  def str[$: P]: P[Expr.Str] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(x => Expr.Str(x.replace("\\n", ""+10.toChar) + "\u0000"))
  def fileName[$: P]: P[Expr.Str] = P("\"" ~~/ CharsWhile(_ != '"', 0).! ~~ "\"").map(x => Expr.Str(x))

  def ident[$: P]: P[Expr.Ident] = P(CharIn("a-zA-Z_") ~~ CharsWhileIn("a-zA-Z0-9_", 0)).!
    .filter(x => !reservedKeywords.contains(x))
    .map((input) => {
      Expr.Ident(input)
    })

  def mod_ident[$: P]: P[Expr.Ident] = P(mod_ident_raw)
    .map((input) => {
      val modules = input._1
      val prefix = if (modules.nonEmpty) modules else file_name
      Expr.Ident(prefix + "_" + input._2)
    })
  def mod_ident_raw[$: P]: P[(String, String)] = P((ident.! ~ "::").rep() ~ ident.!)
    .filter(x => !x._1.exists(y => reservedKeywords.contains(y)) && !reservedKeywords.contains(x._2))
    .map(x=> (x._1.toList.mkString("_"),x._2))
  def mod_ident_no_default[$: P]: P[Expr.Ident] = P(mod_ident_raw)
    .map((input) => {
      val modules = input._1 + "_"
      val prefix = if (modules.length > 1) modules else ""
      Expr.Ident(prefix + input._2)
    })

  def number[$: P]: P[Expr.Num] = P("-".!.? ~~ CharsWhileIn("0-9", 1)).!.map(x => Expr.Num(Integer.parseInt(x)))

  def numberFloat[$: P]: P[Expr.NumFloat] = P("-".? ~~ CharsWhileIn("0-9", 1) ~~ "." ~~ CharsWhileIn("0-9", 1)).!.map(x => Expr.NumFloat(x.toFloat))

  def char[$: P]: P[Expr.Character] = P("'" ~/ !"'" ~ (AnyChar.! | "\\n".!) ~/ "'").map(x => {
    var c = x.charAt(0);
    if(x.length == 2 && x=="\n") c = 10;
    Expr.Character(c)
  });

  def constant[$: P]: P[Expr] = P(trueC | falseC)

  def trueC[$: P]: P[Expr.True] = P("true").map(_ => Expr.True())

  def falseC[$: P]: P[Expr.False] = P("false").map(_ => Expr.False())

  def print[$: P]: P[Expr.Print] = P("print" ~ "(" ~/ (NoCut(binOp) | prefixExpr) ~ ")").map(Expr.Print)
  def free[$: P]: P[Expr.Free] = P("free" ~ "(" ~/ (prefixExpr) ~ ")").map(Expr.Free)

  def throwException[$: P]: P[Expr.ThrowException] = P("throw" ~/ "exception" ~/ "(" ~/ str ~/ ")").map(x => Expr.ThrowException(x.s.dropRight(1)))

  class ParseException(s: String) extends RuntimeException(s)

  val reservedKeywords = List("def", "val", "if", "while", "true", "false", "array", "for", "print", "new", "interface", "return", "object", "throw", "exception", "lambda", "function")

  def checkForReservedKeyword(input: Expr.Ident): Unit = {
    if (reservedKeywords.contains(input.name)) throw new ParseException(s"${input.name} is a reserved keyword");
  }

  var file_name: String = ""
  def parseInput(input: String, _file_name: String): Try[Expr] = {
    val parsed = fastparse.parse(input, topLevel(_), verboseFailures = true);
    parsed match {
      case Parsed.Success(expr, n) => Success(expr.asInstanceOf[Expr]);
      case t: Parsed.Failure => Failure(throw new ParseException(s"parsing fail in $_file_name\n\n${t.trace(true).longAggregateMsg}"))
      
      case _ => throw new ParseException("parsing fail")
    }
  }
}
