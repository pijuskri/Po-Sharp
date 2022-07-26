package posharp

import posharp.Type.{UserType, shortS}

class Counter {
  private var counter = 1;
  private var counterExtra = 0;
  private var paused = false;
  def next(): String = {
    val cur = counter;
    if(!paused) counter += 1;
    s"%$cur";
  }
  def extra(): Int = {
    counterExtra += 1;
    counterExtra - 1;
  }
  def last(): String = s"%${counter-1}";
  def secondLast(): String = s"%${counter-2}";
  def reset(): Unit = {
    counter = 1;
  }
  def pauseToggle(): Unit = {
    paused = !paused;
  }
}

object ToAssembly {
  var varc: Counter = new Counter();
  var ifCounter = 0;
  var subconditionCounter: Int = 0;
  var stringLiterals: List[String] = List()
  var functions: List[FunctionInfo] = List();
  var interfaces: List[InterfaceInfo] = List();
  var enums: List[EnumInfo] = List()
  var lambdas: List[(Expr.Func, Env)] = List()
  var functionScope: FunctionInfo = FunctionInfo("main", List(), Type.Num());

  def convertMain(input: Expr, currentFile: String, otherFiles: Map[String, Expr.TopLevel]): String = {
    ifCounter = 0;
    subconditionCounter = 0;
    stringLiterals = List()
    functions = List();
    interfaces = List();
    enums = List()
    lambdas = List()
    functionScope = FunctionInfo("main", List(), Type.Num());

    /*
    converted += "format_num:\n        db  \"%d\", 10, 0\n"
    converted += "format_float:\n        db  \"%f\", 10, 0\n"
    converted += "format_string:\n        db  \"%s\", 10, 0\n"
    converted += "format_char:\n        db  \"%c\", 10, 0\n"
    converted += "format_true:\n        db  \"true\", 10, 0\n"
    converted += "format_false:\n        db  \"false\", 10, 0\n"
     */
    var converted =
      """
        | declare i32 @printf(i8*, ...)
        | declare i64* @calloc(i32, i32)
        | @format_num = private constant [3 x i8] c"%d\00"
        | @format_float = private constant [3 x i8] c"%f\00"
        | @format_string = private constant [3 x i8] c"%s\00"
        | @format_char = private constant [3 x i8] c"%c\00"
        | @format_false = private constant [7 x i8] c"false\0A\00"
        | @format_true = private constant [7 x i8] c"true\0A\00\00"
        | %Type.array.double = type {i32, double*}
        | %Type.array.i32 = type {i32, i32*}
        | %Type.array.i8 = type {i32, i8*}
        | %Type.array.i1 = type {i32, i1*}
        |
        |""".stripMargin;
    input match { case x: Expr.TopLevel => {
      declareFunctions(x);
      converted += declareInterfaces(x) + "\n";
      /*
      declareEnums(x)
      converted += exportDeclarations(currentFile)
      converted += handleImports(x, otherFiles)
       */
      converted += defineFunctions(x.functions.map(y=>(y, Map())), false);
      converted += defineFunctions(x.interfaces.flatMap(intf=>
        intf.functions.map(func=>Expr.Func(intf.name + "_" + func.name, func.argNames, func.retType, func.body)))
        .map(y=>(y, Map())),
        false
      );
    }}
    //converted += defineFunctions(lambdas, true);
    //converted += "exception:\nmov rdi, 1\ncall exit\n"
    //converted += stringLiterals.mkString
    converted
  }


  private def convert(input: Expr, env: Env): (String, Type) = {
    val conv = (_input: Expr) => convert(_input, env)
    val convt = (_input: Expr, _type: Type) => convert(_input, env) match {
      case (_code, received_type) if received_type == _type => _code
      case (_code, received_type) => throw new Exception(s"got type $received_type, expected ${_type}")
    }
    val ret = input match {
      case Expr.Num(value) => (s"${varc.next()} = add i32 $value, 0\n", Type.Num())
      case Expr.NumFloat(value) => (s"${varc.next()} = fadd double $value, 0.0\n", Type.NumFloat())
      case Expr.Plus(left, right) => aritTemplate(left, right, "add", env)
      case Expr.Minus(left, right) => aritTemplate(left, right, "sub", env)
      case Expr.Mult(left, right) => aritTemplate(left, right, "mul", env)
      case Expr.Div(left, right) => aritTemplate(left, right, "sdiv", env)
      case Expr.Ident(name) => {
        enums.find(x => x.name == name).orElse(interfaces.find(x=>x.name == name)).orElse(Some(lookup(name, env))) match {
          case Some(EnumInfo(_, el)) => ("", Type.Enum(el))
          case Some(InterfaceInfo(itfName, props, funcs)) => ("", Type.Interface(itfName, props, funcs))
          case Some((code: String, variable: Variable)) => (code, variable.varType)
          case _ => throw new Exception(s"unrecognised identifier $name")
        }
      }
      case Expr.True() => (s"${varc.next()} = and i1 1, 1\n", Type.Bool())
      case Expr.False() => (s"${varc.next()} = and i1 0, 0\n", Type.Bool())
      case Expr.Equals(left, right) => (compareExpr(left, right, true, "eq", "oeq", env), Type.Bool())
      case Expr.LessThan(left, right) => (compareExpr(left, right, true, "slt", "olt", env), Type.Bool())
      case Expr.MoreThan(left, right) => (compareExpr(left, right, true, "sgt", "ogt", env), Type.Bool())
      case Expr.Not(left) => {
        val converted = convt(left, Type.Bool())
        val loc = varc.last()
        val ret = converted + s"${varc.next()} = xor i1 $loc, 1\n"
        (ret, Type.Bool())
      }

      case Expr.And(l) => {
        var andLoc = varc.next();
        val ret = l.foldLeft(s"$andLoc = and i1 1, 1\n")((acc, v) => convertLoc(v, env) match {
          case (code, Type.Bool(), loc) =>{
            val ret = acc + code + s"${varc.next()} = and i1 $andLoc, $loc\n"
            andLoc = varc.last()
            ret
          }
          case (_, t, _) => throw new Exception(s"expected bool in and, got $t")
        })
        (ret, Type.Bool())
      }
      case Expr.Or(l) => {
        var andLoc = varc.next();
        val ret = l.foldLeft(s"$andLoc = or i1 0, 0\n")((acc, v) => convertLoc(v, env) match {
          case (code, Type.Bool(), loc) =>{
            val ret = acc + code + s"${varc.next()} = or i1 $andLoc, $loc\n"
            andLoc = varc.last()
            ret
          }
          case (_, t, _) => throw new Exception(s"expected bool in and, got $t")
        })
        (ret, Type.Bool())
      }
      case Expr.DefineArray(size, elemType, defaultValues) => conv(size) match {
        case (code, Type.Num()) => defineArray(code, elemType, defaultValues, env)
        case (_, x) => throw new Exception(s"not number when defining array size, got input of type $x")
      }
      case Expr.GetArray(name, index) => getArray(name, index, env);
      case Expr.ArraySize(name) => getArraySize(name, env);

      case Expr.GetProperty(obj, prop) => convert(obj, env) match {
        case(code, Type.Interface(name, props, funcs)) => props.find(x=>x.name == prop) match {
          case Some(n) => {
            val intfDec = s"%Class.$name"
            val idx = props.indexOf(n);
            val ret = code + s"${varc.next()} = getelementptr $intfDec, $intfDec* ${varc.secondLast()}, i32 0, i32 $idx\n" +
             s"${varc.next()} = load ${Type.toLLVM(n.varType)}, ${Type.toLLVM(n.varType)}* ${varc.secondLast()}\n"
            (ret, n.varType)
          }
          case None => throw new Exception(s"interface ${interfaces.find(x=>x.args == props).get.name} does not have a property ${prop}")
        }
        /*
        //case (code, Type.StaticInterface(props, funcs)) =>
        case (code, Type.Enum(el)) => (s"mov ${reg.head}, 0${el.indexOf(prop)}d\n", Type.Num())
         */
        case (code, Type.Array(a)) if prop == "size" => getArraySize(Expr.Compiled(code, Type.Array(a)), env)//conv(Expr.ArraySize(obj))
        case valType => throw new Exception(s"expected a interface, got ${valType}")
      }
      case Expr.CallF(name, args) => {
        if(functions.exists(x=>x.name == name)) interpFunction(name, args, env)
        //else if(env.contains(name)) callLambda(Expr.Ident(name), args, reg, env)
        else throw new Exception(s"unknown identifier $name")
      }
      case Expr.CallObjFunc(obj, func) => convertType(obj, env) match {
        case Type.Interface(_, props, funcs) => callObjFunction(obj, func, props, funcs, isStatic = false, env)
        case Type.StaticInterface(props, funcs) => callObjFunction(obj, func, props, funcs, isStatic = true, env)
      }
      case Expr.InstantiateInterface(name, values) => interfaces.find(x=>x.name == name) match {
        case Some(intf) => {
          val struct_def = s"${varc.next()} = alloca %Class.$name\n"

          val func_code = interpFunction(name+"_"+name, Expr.Compiled(struct_def, UserType(name)) +: values, env)._1
          val ret = func_code;
          (ret, Type.Interface(name, intf.args, intf.funcs))
        }
        case None => throw new Exception(s"no such interface defined")
      }
      /*
      case Expr.Convert(value, valType: Type) => (convert(value, reg, env), valType) match {
        case ((code, Type.Num()), Type.NumFloat()) => (code + convertToFloat(reg.head), valType)
        case ((code, Type.NumFloat()), Type.Num()) => (code + convertToInt(reg.head), valType)
        case ((code, Type.Num()), Type.Character()) => (code, valType)
        case ((code, l), r) => throw new Exception(s"cant convert from type ${l} to type $r")
      }

      //case Expr.Str(value) => (defineString(value, reg), Type.Str())
      case Expr.Str(value) => (defineArrayKnown(value.length, Type.Character(), value.map(x=>Expr.Character(x)).toList, env)._1, Type.Array(Type.Character()))
      case Expr.Character(value) => (s"mov ${reg.head}, ${value.toInt}\n", Type.Character())

      case Expr.Lambda(args, ret, body) => {
        val label = "lambda_" + lambdas.size
        functions = functions :+ FunctionInfo(label, args, ret)
        lambdas = lambdas :+ (Expr.Func(label, args, ret, body), env)
        (s"mov ${reg.head}, $label\n", Type.Function(args.map(x=>x.varType), ret))
      }
       */
      case Expr.Nothing() => ("", Type.Undefined());
      case Expr.Compiled(code, retType) => (code, retType);
      case Expr.Block(lines) => convertBlock(lines, env);
      case x => throw new Exception (s"$x is not interpreted yet :(");
    }
    (ret._1, makeUserTypesConcrete(ret._2))
  }
  //TODO add line awareness for error reporting
  private def convertBlock(lines: List[Expr], env: Env): (String, Type) = {
    val conv = (_input: Expr) => convert(_input, env)
    if(lines.isEmpty) return ("", Type.Undefined());
    var newenv = env;
    var extendLines = lines;
    var defstring: String = lines.head match {
      case Expr.SetVal(Expr.Ident(name), value) => {
        val look = lookupOffset(name, env)
        val converted = convertLoc(value, env);
        if(makeUserTypesConcrete(look.varType) != converted._2) throw new Exception(s"mismatch when assigning value" +
          s" to variable $name, expected ${look.varType}, but got ${converted._2}")
        val set = s"store ${Type.toLLVM(converted._2)} ${converted._3}, ${Type.toLLVM(converted._2)}* %$name\n"
        converted._1 + set;
      };
      case Expr.DefVal(Expr.Ident(name), varType) => {
        newenv = newVar(name, varType, newenv);
        s"%$name = alloca ${Type.toLLVM(varType)}\n"
      }
      case Expr.If(condition, ifTrue, ifFalse) => {
        def compare(left: Expr, right: Expr, numeric: Boolean): String =
          compareExpr(left, right, numeric, "eq", "oeq", env)
        val trueLabel = s"if_${ifCounter}_true"
        val falseLabel = s"if_${ifCounter}_false"
        val endLabel = s"if_${ifCounter}_end"
        ifCounter += 1;
        val cond = convertType(condition, env) match {
          case Type.Bool() => compare(condition, Expr.True(), false)
          case t => throw new Exception(s"got type $t inside condition, expected bool")
        }
        val ret = cond + s"br i1 ${varc.last()}, label %$trueLabel, label %$falseLabel\n" +
          s"${trueLabel}:\n" + convert(ifTrue, env)._1 + s"br label %$endLabel\n" + s"${falseLabel}:\n" +
          convert(ifFalse, env)._1 + s"br label %$endLabel\n" + s"$endLabel:\n"
        ret
      }
      case Expr.While(condition, execute) => {
        def compare(left: Expr, right: Expr, numeric: Boolean): String =
          compareExpr(left, right, numeric, "eq", "oeq", env)
        val startLabel = s"while_${ifCounter}_start"
        val trueLabel = s"while_${ifCounter}_true"
        val endLabel = s"while_${ifCounter}_end"
        val cond = convertType(condition, env) match {
          case Type.Bool() => compare(condition, Expr.True(), false) +
            s"br i1 ${varc.last()}, label %${trueLabel}, label %${endLabel}\n"
          case t => throw new Exception(s"got type $t inside condition, expected bool")
        }
        val ret = s"br label %${startLabel}\n" + s"${startLabel}:\n" + cond + s"${trueLabel}:\n" +
          convert(execute, env)._1 + s"br label %${startLabel}\n" + s"${endLabel}:\n"
        ifCounter += 1;
        ret
      }
      case Expr.SetArray(expr, index, value) => setArray(expr, index, value, env)
      case Expr.SetInterfaceProp(intf, prop, valueRaw) => convertLoc(intf, env) match {
        case(code, Type.Interface(name, props,f), intfLoc) => props.find(x=>x.name == prop) match {
          case Some(n) => convertLoc(valueRaw, env) match {
            case (valCode, valType, valueLoc) if(valType == n.varType) => {
              val intfDec = s"%Class.$name"
              val idx = props.indexOf(n);
              var ret = code + valCode
              ret += s"${varc.next()} = getelementptr $intfDec, $intfDec* $intfLoc, i32 0, i32 $idx\n"
              ret += s"store ${Type.toLLVM(valType)} $valueLoc, ${Type.toLLVM(n.varType)}* ${varc.last()}\n"
              ret
            }
            case (_, valType, _) => throw new Exception(s"trying to property ${n.name} of type ${n.varType} to incompatible type ${valType}")
          }
          case None => throw new Exception(s"interface ${interfaces.find(x=>x.args == props).get.name} does not have a property ${prop}")
        }
        case (_, valType, _) => throw new Exception(s"expected an interface, got ${valType}")
      }
      /*
      case Expr.ThrowException(err) => {
        val msg = AnsiColor.RED + "RuntimeException: " + err + AnsiColor.RESET
        val name = s"exception_print_${stringLiterals.length}"
        stringLiterals = stringLiterals :+ s"$name:\n        db  \"${msg}\", 10, 0\n"
        s"mov rax, $name\n" + printTemplate("format_string") + "jmp exception\n"
      }
       */
      case x@Expr.CallObjFunc(obj, func) => convert(x, env)._1;
      case x@Expr.CallF(n, a) => convert(x, env)._1;
      case Expr.Return(in) => {
        in match {
          case Some(value) => {
            val converted = convert(value, env)
            if (makeUserTypesConcrete(functionScope.retType) != converted._2)
              throw new Exception(s"Wrong return argument: function ${functionScope.name} expects ${functionScope.retType}, got ${converted._2}")
            converted._1 + s"ret ${Type.toLLVM(converted._2)} ${varc.last()}\n"
          }
          case None => "ret void\n";
        }
        /*
        val defInside = (env.keys.toSet diff functionScope.args.map(x=>x.name).toSet);
        //TODO fix issue when 2 variables reference same location
        val free = "" //freeMemory(env.filter(x=>defInside.contains(x._1)))
        */

      }
      case Expr.Print(toPrint) => printInterp(toPrint, env);
      case x@Expr.Block(n) => convert(x, env)._1;
      case Expr.ExtendBlock(n) => extendLines = extendLines.head +: n ::: extendLines.tail;""
      case _ => throw new Exception(lines.head.toString + " should not be in block lines")
    }
    if(extendLines.tail.nonEmpty) {
      defstring += convertBlock(extendLines.tail, newenv)._1
    }

    (defstring, Type.Undefined());
  }

  def compareExpr(left: Expr, right: Expr, numeric: Boolean, comp: String, fcomp: String, env: Env): String = {
    val leftout = convertLoc(left, env);
    val rightout = convertLoc(right, env);
    var isFloat = false;
    (leftout._2, rightout._2) match {
      case (Type.Bool(), Type.Bool()) if !numeric => ;
      case (Type.Num(), Type.Num()) => ;
      case (Type.NumFloat(), Type.NumFloat()) => isFloat = true;
      case (Type.Character(), Type.Character()) => ;
      case (t1, t2) => throw new Exception(s"can not compare types of $t1 and $t2")
    }
    val cmp = if (isFloat) "fcmp" else "icmp"
    val compKey = if (isFloat) fcomp else comp;
    leftout._1 + rightout._1 + s"${varc.next()} = $cmp $compKey ${Type.toLLVM(leftout._2)} ${leftout._3}, ${rightout._3}\n"
  }

  private def declareFunctions(input: Expr.TopLevel): Unit = {
    functions = input.functions.map(x=> FunctionInfo(x.name, x.argNames, x.retType))
  }
  private def declareInterfaces(input: Expr.TopLevel): String = {
    interfaces = input.interfaces.map(x=> InterfaceInfo(x.name, x.props, x.functions.map(x=>FunctionInfo(x.name,x.argNames,x.retType))))
    functions = functions ::: interfaces.flatMap(x=>addPrefixToFunctions(x.name,x.funcs))
    interfaces.map(intf => {
      val types = intf.args.map(x=>Type.toLLVM(x.varType)).mkString(", ")
      s"%Class.${intf.name} = type {$types}\n"
    }).mkString
  }
  private def addPrefixToFunctions(prefix: String, funcs: List[FunctionInfo]): List[FunctionInfo] = funcs.map(y=>FunctionInfo(prefix+"_"+y.name, y.args, y.retType))
  private def declareEnums(input: Expr.TopLevel): Unit = {
    enums = input.enums.map(x=>EnumInfo(x.name,x.props))
  }
  private def handleImports(input: Expr.TopLevel, otherFiles: Map[String, Expr.TopLevel]): String = {
    input.imports.map(imp=>{
      if (!otherFiles.contains(imp.file)) throw new Exception(s"file \"${imp.file}\" could not be imported");
      val top = otherFiles(imp.file)

      val funcsForImport = searchFileDeclarations(top, imp) match {
        case Expr.Func(name, argnames, retType, code) => {
          functions = functions :+ FunctionInfo(name, argnames, retType)
          List(fNameSignature(FunctionInfo(name, argnames, retType)))
        }
        case Expr.DefineInterface(name, props, i_functions) => {
          val intf = InterfaceInfo(name, props, i_functions.map(x=>FunctionInfo(x.name,x.argNames,x.retType)))
          interfaces = interfaces :+ intf
          val funcs = addPrefixToFunctions(intf.name,intf.funcs)
          functions = functions ::: funcs
          funcs.map(x=>fNameSignature(FunctionInfo(x.name, x.args, x.retType)))
        }
      }
      funcsForImport.map(x=>{
        val label = formatFName(imp.file) + "_" + x
        s"extern $label\n" + s"${x}:\njmp $label\n"
      }).mkString
      /*


      intf.funcs.map(x=>{
            val label = imp.file + "_" + x.name
            s"extern ${label}\n" + s"${x.name}:\njmp ${label}\n"
          }).mkString
       */

    }).mkString
  }
  private def searchFileDeclarations(top: Expr.TopLevel, imp: Expr.Import): Expr = {
    top.functions.find(x=>x.name == imp.toImport)
      .orElse(top.functions.find(x=>x.name == imp.toImport))
      .orElse(top.interfaces.find(x=>x.name == imp.toImport))
      .orElse(throw new Exception(s"could not import ${imp.toImport} from file \"${imp.file}\""))
      .orNull
  }
  private def exportDeclarations(file: String): String = {
    (
      functions
      //interfaces.flatMap(intf=> addPrefixToFunctions(intf.name, intf.funcs))
      )
      .map(info => {
        val formatFile = formatFName(file)
        val name = fNameSignature(info)
        s"global ${formatFile}_${name}\n" + s"${formatFile}_${name}:\njmp ${name}\n"
      }).mkString

  }

  /***
   * Formats file name in a format assembly can understand
   * @param file name
   * @return
   */
  def formatFName(file: String): String = {
    file.replace("/", "_")
  }

  def makeUserTypesConcrete(input: Type): Type = input match {
    case UserType(name) => interfaces.find(x=>x.name == name) match {
      case Some(n) => Type.Interface(name, n.args, n.funcs)
      case _ => throw new Exception (s"no interface of name $name");
    }
    case x => x
  }

  def fNameSignature(info: FunctionInfo): String = fNameSignature(info.name, info.args.map(x=>x.varType))
  def fNameSignature(name: String, args: List[Type]):String = name + (if(args.isEmpty) "" else "_") + args.map(x=>shortS(x)).mkString

  private def defineFunctions(input: List[(Expr.Func, Env)], lambdaMode: Boolean): String = {
    input.map{ case (function, upperScope) => {
      val info = functions.find(x=>x.name == function.name && x.args==function.argNames).get;
      functionScope = info;
      val args = info.args.map(x=>s"${Type.toLLVM(x.varType)} %Input.${x.name}").mkString(", ")
      var ret = s"define ${Type.toLLVM(info.retType)} @${info.name}($args) {\n"
      val newEnv = upperScope ++ info.args.map(x=> (x.name, Variable(x.varType))).toMap
      var body = info.args.map(x=>
        s"%${x.name} = alloca ${Type.toLLVM(x.varType)}\n" +
        s"store ${Type.toLLVM(x.varType)} %Input.${x.name}, ${Type.toLLVM(x.varType)}* %${x.name}\n").mkString

      body += convert(function.body, newEnv)._1

      if(info.retType == Type.Undefined()) body += "ret void\n"
      if(info.name == "main") body += "ret i32 0\n"
      varc.reset()

      ret += body.split("\n").map(x=>"\t"+x).mkString("\n")
      ret += "\n}\n"
      ret
    }}.mkString
  }
  def interpFunction(name: String, args: List[Expr], env: Env ): (String, Type) = {
    val argRet = args.map (arg => convertLoc(arg, env))
    val argInputTypes = argRet.map(x => x._2)
    var ret = argRet.map(x=>x._1).mkString
    val argsString = argRet.map(x=>s"${Type.toLLVM(x._2)} ${x._3}").mkString(", ")

    functions.find(x=>x.name == name) match {
      case Some(x) => ; case None => throw new Exception(s"function of name $name undefined");
    }
    functions.find(x=>x.name == name && Type.compare(argInputTypes, x.args.map(x=>makeUserTypesConcrete(x.varType)))) match {
      case Some(FunctionInfo(p, argTypes, retType)) => {
        val argTypeString = argTypes.map(x=>Type.toLLVM(x.varType)).mkString(", ")
        if (retType != Type.Undefined()) ret += s"${varc.next()} = ";
        ret += s"call ${Type.toLLVM(retType)} ($argTypeString) @$name($argsString)\n"
        (ret, retType)
      }
      case None => {
        println(functions.find(x=>x.name == name).map(x=>x.args).mkString);
        throw new Exception(s"no overload of function $name matches argument list $argInputTypes")
      };
    }
  }

  def callObjFunction(obj: Expr, func: Expr.CallF, props: List[InputVar], funcs: List[FunctionInfo], isStatic: Boolean, env: Env): (String, Type) = {
    funcs.find(x=>x.name == func.name) match {
      case Some(n) => {
        var args = func.args
        //TODO fails if interface has same attributes/functions but different name
        val intfName = interfaces.find(x=>x.args == props && x.funcs == funcs).get.name
        if(n.args.nonEmpty && n.args.head.name == "self") {
          if(isStatic) throw new Exception(s"can not call method $func staticly")
          else args = obj +: args
        }
        interpFunction(intfName+"_"+func.name, args, env)
      }
      case None => props.find(x=>x.name == func.name) match {
        case Some(InputVar(_, Type.Function(_,_))) => {
          //callLambda(Expr.GetProperty(obj, func.name), func.args, reg, env)
          ("",Type.Undefined())
        }
        case None => throw new Exception(s"object has no property or function named $func")
      }
    }
  }
  /*
  def callLambda(input: Expr, args: List[Expr], reg: List[String], env: Env): (String, Type) = convert(input, reg, env) match {
    case (code, Type.Function(argTypes, retType)) => {
      val usedReg = defaultReg.filter(x => !reg.contains(x));
      var ret = usedReg.map(x=>s"push $x\n").mkString
      ret += code + s"push ${reg.head}\n"
      if(argTypes.length != args.length) throw new Exception(s"wrong number of arguments: expected ${argTypes.length}, got ${args.length}");
      val argRet = (args zip argTypes).map{case (arg, argType) => convert(arg, reg, env) match {
        case (argCode, t) if Type.compare(t, argType) => argCode + s"push ${reg.head}\n"
        case (_, t) => throw new Exception(s"Wrong argument for function: expected $argType, got $t");
      }}
      ret += argRet.mkString
      ret += args.zipWithIndex.reverse.map{case (arg, index) => s"pop ${functionCallReg(index)}\n"}.mkString
      ret += s"pop rax\n"
      ret += s"call rax\n"
      ret += s"mov ${reg.head}, rax\n"
      ret += usedReg.reverse.map(x=>s"pop $x\n").mkString
      (ret, retType)
    }
    case (_, x) => throw new Exception(s"Can not call variable of type $x");
  }

  def defineString(value: String, reg: List[String]): String = {
    //TODO Make definitions dynamic also
    //var ret = s"mov rdi, ${value.length+1}\n" + s"mov rsi, 8\n" + "call calloc\n" + "push rax\n" + "mov r9, rax\n";
    val label = s"string_${stringLiterals.length}";
    stringLiterals = stringLiterals :+ s"$label:\n        db  \"${value}\", 10, 0\n"
    s"mov ${reg.head}, $label\n"
  }
   */
  def getArrayPointerIndex(arrType: Type, arrLoc: String, indexLoc: String): String = {
    val Tsig = Type.toLLVM(arrType)
    val arrTC = s"%Type.array.$Tsig"

    var ret = "";
    ret += s"${varc.next()} = getelementptr $arrTC, $arrTC* $arrLoc, i32 0, i32 1\n"
    val arrStructLoc = varc.last()
    ret += s"${varc.next()} = getelementptr inbounds $Tsig*, $Tsig** ${arrStructLoc}, i32 $indexLoc\n"
    val arrElemLoc = varc.last()
    ret += s"${varc.next()} = load $Tsig*, $Tsig** ${arrElemLoc}\n"
    ret
  }
  def getArray(arr: Expr, index: Expr, env: Env): (String, Type) = (convertLoc(arr, env), convertLoc(index, env)) match {
    case ((code, Type.Array(arrType), arrLoc), (indexCode, indexType, indexLoc)) => {
      if(indexType != Type.Num()) throw new Exception(s"wrong index for array, got $indexType");
      val Tsig = Type.toLLVM(arrType)

      var ret = code + indexCode;
      ret += getArrayPointerIndex(arrType, arrLoc, indexLoc)
      ret += s"${varc.next()} = load $Tsig, $Tsig* ${varc.secondLast()}\n"
      (ret, arrType)
    }
    case ((_, varType, _), _) => throw new Exception(s"trying to access variable ${arr} as an array, has type $varType")
  }
  def setArray(arr: Expr, index: Expr, newVal: Expr, env: Env): String =
    (convertLoc(arr, env), convertLoc(index, env), convertLoc(newVal, env)) match {
      case ((code, Type.Array(arrType), arrLoc), (indexCode, indexType, indexLoc), (valCode, valType, valLoc)) => {
        if(arrType != valType) throw new Exception(s"trying to set array element of type ${arrType} to $valType");
        if(indexType != Type.Num()) throw new Exception(s"wrong index for array, got $indexType");
        val Tsig = Type.toLLVM(arrType)

        var ret = code + indexCode + valCode;
        ret += getArrayPointerIndex(arrType, arrLoc, indexLoc)
        ret += s"store $Tsig $valLoc, $Tsig* ${varc.last()}\n"
        ret
    }
  }
  def getArraySize(arr: Expr, env: Env): (String, Type) = convertLoc(arr, env) match {
    case (code, Type.Array(arrType), loc) => {
      val Tsig = Type.toLLVM(arrType)
      val arrTC = s"%Type.array.$Tsig"

      val ret = code +
        s"${varc.next()} = getelementptr $arrTC, $arrTC* $loc, i32 0, i32 0\n" +
        s"${varc.next()} = load $Tsig, $Tsig* ${varc.secondLast()}\n"
      (ret, Type.Num())
    }
    case (_, varType, _) => throw new Exception(s"trying to access variable ${arr} as an array, has type $varType")
  }

  def defineArray(size: String, setElemType:Type, defaultValues: List[Expr], env: Env): (String, Type) = {
    var elemType: Type = setElemType;
    val array_elem_size = arraySizeFromType(elemType);
    val Tsig = Type.toLLVM(elemType)
    val arrTC = s"%Type.array.$Tsig"
    var ret = size;
    val sizeLoc = varc.last()
    ret += s"${varc.next()} = call i64* (i32, i32) @calloc(i32 $sizeLoc, i32 $array_elem_size)\n";
    ret += s"${varc.next()} = bitcast i64* ${varc.secondLast()} to $Tsig*"
    val arrLoc = varc.last()
    val sizeStructPointer = s"%arr.size.${varc.extra()}"
    val arrStructPointer = s"%arr.arr.${varc.extra()}"
    ret += s"${varc.next()} = alloca $arrTC\n"
    val structLoc = varc.last()

    ret += s"${sizeStructPointer} = getelementptr $arrTC, $arrTC* $structLoc, i32 0, i32 0\n"
    ret += s"store i32 $sizeLoc, i32* ${sizeStructPointer}\n"
    ret += s"${arrStructPointer} = getelementptr $arrTC, $arrTC* $structLoc, i32 0, i32 1\n"
    ret += s"store $Tsig* $arrLoc, $Tsig** ${arrStructPointer}\n"
    /*
    var ret = defaultValues.zipWithIndex.map{case (entry, index) => {
      val converted = convert(entry, defaultReg, env)
      if(elemType == Type.Undefined()) elemType = converted._2;
      else if(converted._2 != elemType) throw new Exception(s"array elements are of different types")
      converted._1 + setArrayDirect("[rsp]", skipArrSize(index, arraySizeFromType(elemType)), arraySizeFromType(elemType));
    }}.mkString;
     */
    (ret, Type.Array(elemType))
  }
  def arraySizeFromType(valtype: Type): Int = valtype match {
    case Type.Undefined() => 8
    case Type.Character() => 1
    case Type.Num() => 8
    case Type.NumFloat() => 8
    case Type.Function(_,_) => 8
    case Type.T1() => 8
  }

  def lookup(tofind: String, env: Env): (String, Variable) = {
    val ret = lookupOffset(tofind, env)
    (s"${varc.next()} = load ${Type.toLLVM(ret.varType)}, ${Type.toLLVM(ret.varType)}* %$tofind\n", ret)
  }
  def lookupOffset(tofind: String, env: Env): Variable = env.get(tofind) match {
    case Some(v) => v
    case None => throw new Exception(s"variable \"${tofind}\" undefined")
  }
  def newVar(name: String, varType: Type, env: Env) : Env = {
    if(env.contains(name)) throw new Exception(s"variable \"${name}\" already defined")
    env + (name -> Variable(varType))
  }

  def convertLoc(input: Expr, env: Env): (String, Type, String) = {
    val ret = convert(input, env)
    (ret._1, ret._2, varc.last())
  }
  def convertType(input: Expr, env: Env): Type = {
    varc.pauseToggle()
    val ret = convert(input, env)._2
    varc.pauseToggle()
    ret
  }
  def intBinOpTemplate(codeLeft: String, vLeft: String, codeRight: String, vRight: String, command: String): (String, Type) = {
    (codeLeft + codeRight + s"${varc.next()} = $command i32 $vLeft, $vRight\n", Type.Num());
  }
  def floatBinOpTemplate(codeLeft: String, vLeft: String, codeRight: String, vRight: String, command: String): (String, Type) = {
    (codeLeft + codeRight + s"${varc.next()} = f$command double $vLeft, $vRight\n", Type.NumFloat());
  }

  def aritTemplate(left: Expr, right: Expr, command: String, env: Env): (String, Type) = {
    (convertLoc(left, env), convertLoc(right, env)) match {
      case ((codeLeft, Type.Num(), vLeft), (codeRight, Type.Num(), vRight)) =>
        intBinOpTemplate(codeLeft, vLeft, codeRight, vRight, command)
      case ((codeLeft, Type.NumFloat(), vLeft), (codeRight, Type.NumFloat(), vRight)) =>
        if(command == "sdiv") floatBinOpTemplate(codeLeft, vLeft, codeRight, vRight, "div")
        else floatBinOpTemplate(codeLeft, vLeft, codeRight, vRight, command)
      //case ((codeLeft, Type.Num()), (codeRight, Type.NumFloat())) => floatTemplate(codeLeft + convertToFloat(reg.head), codeRight, commandFloat, reg)
      //case ((codeLeft, Type.NumFloat()), (codeRight, Type.Num())) => floatTemplate(codeLeft, codeRight + convertToFloat(reg.tail.head), commandFloat, reg)
      case ((codeLeft, typeLeft, x), (codeRight, typeRight, y)) => throw new Exception(s"can't perform arithmetic on operands of types ${typeLeft} and ${typeRight}");
    }
  }
  /*
  def convertToFloat(reg: String): String = {
    s"cvtsi2sd xmm0, ${reg}\n" + s"movq ${reg}, xmm0\n"
  }
  def convertToInt(reg: String): String = {
    s"movq xmm0, ${reg}\n" + s"cvtsd2si ${reg}, xmm0\n"
  }
  */

  def printTemplate(format: String, ty: String): String = {
    s"%ret.${varc.extra()} = call i32 (i8*, ...) @printf(i8* getelementptr inbounds" +
      s" ([3 x i8], [3 x i8]* @$format, i32 0, i32 0), $ty ${varc.last()})\n"
  }
  def printInterp(toPrint: Expr, env: Env): String = {
    val converted = convert(toPrint, env)
    ifCounter+=1
    converted._2 match {
      case Type.Num() => converted._1 + printTemplate("format_num", "i32");
      case Type.Bool() => converted._1 + printTemplate("format_num", "i1");
      case Type.NumFloat() => converted._1 + printTemplate("format_float", "double");
      /*
      //case (Type.Str) => converted._1 + printTemplate("format_string");
      case Type.Bool() => converted._1 + s"cmp rax, 0\nje bool_${ifCounter}\n" + printTemplate("format_true") +
        s"jmp boole_${ifCounter}\nbool_${ifCounter}:\n" + printTemplate("format_false") + s"boole_${ifCounter}:\n";
      case Type.Character() => converted._1 + printTemplate("format_char");
      case Type.Array(Type.Character()) => converted._1 + "add rax, 8\n" + printTemplate("format_string");
       */
      case _ => throw new Exception(s"input of type ${converted._2} not recognized in print")
    }
  }
  //TODO runtime memory garbage collection, currently only pointers on the stack are handled
  /*
  def freeMemory(env: Env): String = env.foldLeft("")((acc, entry) => entry._2.varType match {
    case Type.Array(arrType) => acc + s"mov rdi, [rbp-${entry._2.pointer}]\n" + "call free\n";
    //case Type.Interface(args) => acc + s"mov rdi, [rbp-${entry._2.pointer}]\n" + "call free\n";
    case _ => acc
  })

   */

  type Env = Map[String, Variable]
  case class FunctionInfo(name: String, args: List[InputVar], retType: Type)
  //case class InterfaceInfo(name: String, args: List[InputVar])
  case class InterfaceInfo(name: String, args: List[InputVar], funcs: List[FunctionInfo])
  case class EnumInfo(name: String, el: List[String])
  case class Variable(varType: Type)
}

