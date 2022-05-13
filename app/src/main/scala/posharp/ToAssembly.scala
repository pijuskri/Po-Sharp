package posharp

import Expr.GetProperty
import Type.{UserType, shortS}
import scala.io.AnsiColor

object ToAssembly {
  val defaultReg = List("rax", "rdi", "rsi", "rdx", "rcx", "r8", "r9", "r10", "r11")
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
        | extern exit
        | section .text
        |""".stripMargin;
    input match { case x: Expr.TopLevel => {
      declareFunctions(x);
      declareInterfaces(x);
      declareEnums(x)
      converted += exportDeclarations(currentFile)
      converted += handleImports(x, otherFiles)
      converted += defineFunctions(x.functions.map(y=>(y, Map())), false);
      converted += defineFunctions(x.interfaces.flatMap(intf=>
        intf.functions.map(func=>Expr.Func(intf.name + "_" + func.name, func.argNames, func.retType, func.body)))
        .map(y=>(y, Map())),
        false
      );
    }}
    converted += defineFunctions(lambdas, true);
    converted += "exception:\nmov rdi, 1\ncall exit\n"
    converted += "format_num:\n        db  \"%d\", 10, 0\n"
    converted += "format_float:\n        db  \"%f\", 10, 0\n"
    converted += "format_string:\n        db  \"%s\", 10, 0\n"
    converted += "format_char:\n        db  \"%c\", 10, 0\n"
    converted += "format_true:\n        db  \"true\", 10, 0\n"
    converted += "format_false:\n        db  \"false\", 10, 0\n"
    //converted += "section .data\nmain_rbp	DQ	0\nmain_rsp	DQ	0\n"
    converted += stringLiterals.mkString
    //converted = converted.split("\n").zipWithIndex.foldLeft("")((acc, v)=> acc +s"\nline${v._2}:\n"+ v._1)
    converted = converted.split("\n").map(x=>if(x.contains(":")) x+"\n" else "   "+x+"\n").mkString
    converted
  }


  private def convert(input: Expr, reg: List[String], env: Env): (String, Type) = {
    val conv = (_input: Expr) => convert(_input, reg, env)
    val convt = (_input: Expr, _type: Type) => convert(_input, reg, env) match {
      case (_code, received_type) if received_type == _type => _code
      case (_code, received_type) => throw new Exception(s"got type $received_type, expected ${_type}")
    }
    val ret = input match {
      case Expr.Num(value) => (s"mov ${reg.head}, 0${value}d\n", Type.Num())
      case Expr.NumFloat(value) => {
        (s"mov ${reg.head}, __float64__(${value.toString})\n", Type.NumFloat())
      }
      case Expr.True() => (s"mov ${reg.head}, 1\n", Type.Bool())
      case Expr.False() => (s"mov ${reg.head}, 0\n", Type.Bool())
      case Expr.Plus(left, right) => aritTemplate(left, right, "add", "addsd", reg, env)
      case Expr.Minus(left, right) => aritTemplate(left, right, "sub", "subsd", reg, env)
      case Expr.Mult(left, right) => aritTemplate(left, right, "mul", "mulsd", reg, env)
      case Expr.Div(left, right) => aritTemplate(left, right, "div", "divsd", reg, env)
      case Expr.Convert(value, valType: Type) => (convert(value, reg, env), valType) match {
        case ((code, Type.Num()), Type.NumFloat()) => (code + convertToFloat(reg.head), valType)
        case ((code, Type.NumFloat()), Type.Num()) => (code + convertToInt(reg.head), valType)
        case ((code, Type.Num()), Type.Character()) => (code, valType)
        case ((code, l), r) => throw new Exception(s"cant convert from type ${l} to type $r")
      }
      case Expr.Ident(name) => {
        val isStatic = enums.find(x=>x.name == name)
        if(isStatic.nonEmpty) {
          val enumInfo = isStatic.get
          ("", Type.Enum(enumInfo.el))
        }
        else {
          val look = lookup(name, env)
          (s"mov ${reg.head}, ${look._1}\n", look._2.varType)
        }
      }
      case Expr.Block(lines) => convertBlock(lines, reg, env);
      case Expr.DefineArray(size, elemType, defaultValues) => conv(size) match {
        case (code, Type.Num()) => defineArray(code, elemType, defaultValues, env)
        case (code, x) => throw new Exception(s"not number when defining array size, got input of type $x")
      }
      /*
      case Expr.InstantiateInterface(name, values) => interfaces.find(x=>x.name == name) match {
        case Some(intf) => {
          var ret = values.zipWithIndex.map{case (entry, index) => {
            val converted = convert(entry, defaultReg, env);
            if(converted._2 != intf.args(index).varType) throw new Exception(s"expected type ${intf.args(index).varType}" +
              s" for interface element ${intf.args(index).name}, but got ${converted._2}")
            converted._1 + setArrayDirect("[rsp]", index, 8);
          }}.mkString;
          val array_def = s"mov rdi, ${intf.args.length}\n" + s"mov rsi, 8\n" + "call calloc\n" + "push rax\n"
          ret = array_def + ret + "pop rax\n";
          (ret, Type.Interface(intf.args))
        }
        case None => throw new Exception(s"no such interface defined")
      }
       */
      case Expr.InstantiateInterface(name, values) => interfaces.find(x=>x.name == name) match {
        case Some(intf) => {
          val array_def = s"mov rdi, ${intf.args.length}\n" + s"mov rsi, 8\n" + "call calloc\n" + "push rax\n"
          //val newenv = newVar("self", UserType(name), env)
          //val func_code = interpFunction(name+"_"+name, Expr.Ident("self") +: values, reg, newenv)._1
          //val ret = array_def + setval("self", UserType(name), newenv)._1 + func_code + "pop rax\n";

          val func_code = interpFunction(name+"_"+name, Expr.Compiled(array_def, UserType(name)) +: values, reg, env)._1
          val ret = func_code + "pop rax\n";
          (ret, Type.Interface(intf.args, intf.funcs))
        }
        case None => throw new Exception(s"no such interface defined")
      }
      case Expr.GetProperty(obj, prop) => conv(obj) match {
        case(code, Type.Interface(props, funcs)) => props.find(x=>x.name == prop) match {
          case Some(n) => {
            val ret = code + getArrayDirect(reg.head, props.indexOf(n), 8, reg)
            (ret, n.varType)
          }
          case None => throw new Exception(s"interface ${interfaces.find(x=>x.args == props).get.name} does not have a property ${prop}")
        }
        case (code, Type.Enum(el)) => (s"mov ${reg.head}, 0${el.indexOf(prop)}d\n", Type.Num())
        case (code, Type.Array(a)) if prop == "size" => conv(Expr.ArraySize(obj))
        case (x, valType) => throw new Exception(s"expected a interface, got ${valType}")
      }
      case Expr.CallObjFunc(obj, func) => conv(obj) match {
        case(code, t@Type.Interface(props, funcs)) => funcs.find(x=>x.name == func.name) match {
          case Some(n) => {
            var args = func.args
            //TODO fails if interface has same attributes/functions but different name
            val intfName = interfaces.find(x=>x.args == props && x.funcs == funcs).get.name
            if(n.args.nonEmpty && n.args.head.name == "self") args = obj +: args
            interpFunction(intfName+"_"+func.name, args, reg, env)
          }
          case None => props.find(x=>x.name == func.name) match {
            case Some(InputVar(_, Type.Function(_,_))) => {
              //callLambda(Expr.GetProperty(Expr.Compiled(code, t), func.name), func.args, reg, env)
              callLambda(Expr.GetProperty(obj, func.name), func.args, reg, env)
            }
            case None => throw new Exception(s"object has no property or function named $func")
          }
        }
      }
      case Expr.GetArray(name, index) => getArray(name, index, reg, env);
      case Expr.ArraySize(name) => getArraySize(name, reg, env);
      case Expr.CallF(name, args) => {
        if(functions.exists(x=>x.name == name)) interpFunction(name, args, reg, env)
        else if(env.contains(name)) callLambda(Expr.Ident(name), args, reg, env)
        else throw new Exception(s"unknow identifier $name")
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
      case Expr.Equals(left, right) => {
        val ret = compareExpr(left, right, false, reg, env) + s"sete ${sizeToReg(1, reg.head)}\n"
        (ret, Type.Bool())
      }
      case Expr.LessThan(left, right) => {
        val ret = compareExpr(left, right, true, reg, env) + s"setl ${sizeToReg(1, reg.head)}\n"
        (ret, Type.Bool())
      }
      case Expr.MoreThan(left, right) => {
        val ret = compareExpr(left, right, true, reg, env) + s"setg ${sizeToReg(1, reg.head)}\n"
        (ret, Type.Bool())
      }
      case Expr.Not(left) => {
        val ret = convt(left, Type.Bool()) + s"xor ${reg.head}, 1\n"
        (ret, Type.Bool())
      }
      case Expr.And(l) => {
        val reg1 = sizeToReg(1, reg.head)
        val reg2 = sizeToReg(1, reg.tail.head)
        val ret = l.foldLeft(s"mov ${reg.head}, 1\n")((acc, v) => convert(v, reg.tail, env) match {
          case (code, Type.Bool()) => acc + code + s"and ${reg1}, ${reg2}\n" //cmp ${reg1}, 1\nsete $reg1\n
          case (_, t) => throw new Exception(s"expected bool in and, got $t")
        })
        (ret, Type.Bool())
      }
      case Expr.Or(l) => {
        val reg1 = sizeToReg(1, reg.head)
        val reg2 = sizeToReg(1, reg.tail.head)
        val ret = l.foldLeft(s"mov ${reg.head}, 0\n")((acc, v) => convert(v, reg.tail, env) match {
          case (code, Type.Bool()) => acc + code + s"or ${reg1}, ${reg2}\n"
          case (_, t) => throw new Exception(s"expected bool in and, got $t")
        })
        (ret, Type.Bool())
      }

      case Expr.Nothing() => ("", Type.Undefined());
      case Expr.Compiled(code, retType) => (code, retType);
      case x => throw new Exception (s"$x is not interpreted yet :(");
    }
    (ret._1, makeUserTypesConcrete(ret._2))
  }
  //TODO add line awareness for error reporting
  private def convertBlock(lines: List[Expr], reg: List[String], env: Env): (String, Type) = {
    val conv = (_input: Expr) => convert(_input, reg, env)
    if(lines.isEmpty) return ("", Type.Undefined());
    var newenv = env;
    var extendLines = lines;
    var defstring: String = lines.head match {
      case Expr.SetVal(Expr.Ident(name), value) => {
        val converted = convert(value, reg, env);
        val modified = setval(name, converted._2, env)
        newenv = modified._2
        converted._1 + modified._1
      };
      case Expr.SetArray(expr, index, value) => {
        val converted = convert(value, reg, env);
        val arr = setArray(expr, index, converted._2, env)
        converted._1 + arr
      };
      case Expr.SetInterfaceProp(intf, prop, valueRaw) => convert(intf, reg.tail, env) match {
        case(code, Type.Interface(props,f)) => props.find(x=>x.name == prop) match {
          case Some(n) => conv(valueRaw) match {
            case (valCode, valType) if(valType == n.varType) =>
              valCode + code + setArrayDirect(s"${reg.tail.head}", props.indexOf(n), 8)
            case (_, valType) => throw new Exception(s"trying to property ${n.name} of type ${n.varType} to incompatible type ${valType}")
          }
          case None => throw new Exception(s"interface ${interfaces.find(x=>x.args == props).get.name} does not have a property ${prop}")
        }
        case (x, valType) => throw new Exception(s"expected a interface, got ${valType}")
      }
      case Expr.DefVal(Expr.Ident(name), varType) => newenv = newVar(name, varType, newenv); ""
      case Expr.Print(toPrint) => printInterp(toPrint, env);
      case Expr.If(condition, ifTrue, ifFalse) => {
        def compare(left: Expr, right: Expr, numeric: Boolean): String = compareExpr(left, right, numeric, reg, env)
        val trueLabel = s"if_${ifCounter}_true"
        val falseLabel = s"if_${ifCounter}_false"
        val endLabel = s"if_${ifCounter}_end"
        ifCounter += 1;
        val cond = convert(condition, reg, env) match {
          case (code, Type.Bool()) => compare(condition, Expr.True(), false) + s"jne ${falseLabel}\n"
          case (_, t) => throw new Exception(s"got type $t inside condition, expected bool")
        }
        //convertCondition(condition, reg, env, orMode = false, trueLabel, falseLabel)
        val ret = cond + s"${trueLabel}:\n" + convert(ifTrue, reg, env)._1 +
          s"jmp ${endLabel}\n" + s"${falseLabel}:\n" + convert(ifFalse, reg, env)._1 + s"${endLabel}:\n"
        ret
      }
      case Expr.While(condition, execute) => {
        def compare(left: Expr, right: Expr, numeric: Boolean): String = compareExpr(left, right, numeric, reg, env)
        val startLabel = s"while_${ifCounter}_start"
        val trueLabel = s"while_${ifCounter}_true"
        val endLabel = s"while_${ifCounter}_end"
        val cond = convert(condition, reg, env) match {
          case (code, Type.Bool()) => compare(condition, Expr.True(), false) + s"jne ${endLabel}\n"
          case (_, t) => throw new Exception(s"got type $t inside condition, expected bool")
        }
        val ret = s"${startLabel}:\n" + cond + s"${trueLabel}:\n" + convert(execute, reg, env)._1 +
          s"jmp ${startLabel}\n" + s"${endLabel}:\n"
        ifCounter += 1;
        ret
      }
      case Expr.Return(in) => {
        val defInside = (env.keys.toSet diff functionScope.args.map(x=>x.name).toSet);
        //TODO fix issue when 2 variables reference same location
        val free = "" //freeMemory(env.filter(x=>defInside.contains(x._1)))
        in match {
          case Some(value) => {
            val converted = convert(value, defaultReg, env)
            if (makeUserTypesConcrete(functionScope.retType) != converted._2)
              throw new Exception(s"Wrong return argument: function ${functionScope.name} expects ${functionScope.retType}, got ${converted._2}")
            converted._1 + free + "leave\nret\n"
          }
          case None => free + "leave\nret\n";
        }

      }
      case Expr.ThrowException(err) => {
        val msg = AnsiColor.RED + "RuntimeException: " + err + AnsiColor.RESET
        val name = s"exception_print_${stringLiterals.length}"
        stringLiterals = stringLiterals :+ s"$name:\n        db  \"${msg}\", 10, 0\n"
        s"mov rax, $name\n" + printTemplate("format_string") + "jmp exception\n"
      }
      case x@Expr.CallF(n, a) => convert(x, reg, env)._1;
      case x@Expr.Block(n) => convert(x, reg, env)._1;
      case x@Expr.CallObjFunc(obj, func) => convert(x, reg, env)._1;
      case Expr.ExtendBlock(n) => extendLines = extendLines.head +: n ::: extendLines.tail;""
      case _ => throw new Exception(lines.head.toString + " should not be in block lines")
    }
    if(extendLines.tail.nonEmpty) {
      defstring += convertBlock(extendLines.tail, defaultReg, newenv)._1
    }
    //defstring += freeMemory((newenv.toSet diff env.toSet).toMap)
    (defstring, Type.Undefined());
  }

  def compareExpr(left: Expr, right: Expr, numeric: Boolean, reg: List[String], env: Env): String = {
    val leftout = convert(left, reg, env);
    val rightout = convert(right, reg.tail, env);
    (leftout._2, rightout._2) match {
      case (Type.Bool(), Type.Bool()) if !numeric => ;
      case (Type.Num(), Type.Num()) => ;
      case (Type.NumFloat(), Type.NumFloat()) => ;
      case (Type.Character(), Type.Character()) => ;
      case (t1, t2) => throw new Exception(s"can not compare types of $t1 and $t2")
    }
    leftout._1 + rightout._1 + s"cmp ${reg.head}, ${reg.tail.head}\n"
  }
  private def declareFunctions(input: Expr.TopLevel): Unit = {
    functions = input.functions.map(x=> FunctionInfo(x.name, x.argNames, x.retType))
  }
  private def declareInterfaces(input: Expr.TopLevel): Unit = {
    interfaces = input.interfaces.map(x=> InterfaceInfo(x.name, x.props, x.functions.map(x=>FunctionInfo(x.name,x.argNames,x.retType))))
    /*
    interfaces = interfaces.map(x=>
      InterfaceInfo( x.name, x.args.map(y=>
        InputVar(y.name, traverseTypeTree(y.varType))
      ), x.funcs.map(y =>
        FunctionInfo(y.name,
          y.args.map(z=>InputVar(z.name, traverseTypeTree(z.varType))),
          traverseTypeTree(y.retType))
      ))
    )
     */
    functions = functions ::: interfaces.flatMap(x=>addPrefixToFunctions(x.name,x.funcs))
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
      case Some(n) => Type.Interface(n.args, n.funcs)
      case _ => throw new Exception (s"no interface of name $name");
    }
    case x => x
  }
  //TODO avoid traversing the same interfaces by creating a list of marking which interfaces are concretely defined
  /*
  def traverseTypeTree(input: Type): Type = input match {
    case UserType(name) => interfaces.find(x=>x.name == name) match {
      case Some(n) => traverseTypeTree(Type.Interface(n.args, n.funcs)) match {
        case t@Type.Interface(newargs,f) =>
          interfaces = interfaces.map(x=>if(x == n) InterfaceInfo(x.name, newargs, x.funcs) else x)
          t
      }
      case _ => throw new Exception (s"no interface of name $name");
    }
    case Type.Interface(args,f) => Type.Interface(args.map(x=>InputVar(x.name, traverseTypeTree(x.varType))), f)
    case Type.Array(valType) => traverseTypeTree(valType)
    case x => x
  }

   */
  val functionCallReg = List( "rdi", "rsi", "rdx", "rcx", "r8", "r9")
  def fNameSignature(info: FunctionInfo): String = fNameSignature(info.name, info.args.map(x=>x.varType))
  def fNameSignature(name: String, args: List[Type]):String = name + (if(args.isEmpty) "" else "_") + args.map(x=>shortS(x)).mkString

  private def defineFunctions(input: List[(Expr.Func, Env)], lambdaMode: Boolean): String = {
    input.map{ case (function, upperScope) => {
      val info = functions.find(x=>x.name == function.name && x.args==function.argNames).get;
      functionScope = info;
      val label = if(lambdaMode) info.name else fNameSignature(info.name, info.args.map(x=>x.varType))
      var ret = "\n" + s"${label}:\n"
      //if(info.name == "main") ret += "mov [main_rbp], rbp\nmov [main_rsp], rsp\n"
      ret +=
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
      ret += convert(function.body, defaultReg, shiftEnvLocations(upperScope) ++ env)._1
      if(info.retType == Type.Undefined()) ret += "leave\nret\n";
      if(info.name == "main") {
        //ret += "exception:\nmov rdi, 1\nmov rbp, [main_rbp]\nmov rsp, [main_rsp]\nmov rdx, [rsp-8]\njmp rdx\n"
        ret += "mov rax, 0\nleave\nret\n";
      }

      ret
    }}.mkString
  }
  def shiftEnvLocations(env: Env): Env = {
      env.map(x=> (x._1,
        Variable(x._2.pointer - 256 - 16 , x._2.varType)
      ))
  }
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
  def interpFunction(name: String, args: List[Expr], reg: List[String], env: Env ): (String, Type) = {
    val usedReg = defaultReg.filter(x => !reg.contains(x));
    var ret = usedReg.map(x=>s"push $x\n").mkString
    val argRet = args.zipWithIndex.map{case (arg, index) => {
      val converted = convert(arg, reg, env)
      (converted._1 + s"push ${reg.head}\n", converted._2)
    }}
    val argInputTypes = argRet.map(x=>x._2)
    ret += argRet.map(x=>x._1).mkString
    ret += args.zipWithIndex.reverse.map{case (arg, index) => s"pop ${functionCallReg(index)}\n"}.mkString

    //if(converted._2 != argTypes(index)) throw new Exception (s"wrong argument type: expected ${argTypes(index)}, got ${converted._2}")
    functions.find(x=>x.name == name) match {
      case Some(x) => ; case None => throw new Exception(s"function of name $name undefined");
    }
    functions.find(x=>x.name == name && Type.compare(argInputTypes, x.args.map(x=>makeUserTypesConcrete(x.varType)))) match {
      case Some(FunctionInfo(p, argTypes, retType)) => {
        //if(argTypes.length != args.length) throw new Exception (s"wrong number of arguments: expected ${argTypes.length}, got ${args.length}")
        //TODO add errors for unexpected behaviour
        def eqT(x: Type): Boolean = x == Type.T1() || x == Type.Array(Type.T1())
        val template1Type = argTypes.map(x=>x.varType).zipWithIndex.find(x=> eqT(x._1)) match {
          case Some(n) => argInputTypes(n._2)
          case None => Type.Undefined()
        }
        argTypes.foreach(x=> {
          if (eqT(x.varType) && !Type.compare(x.varType, template1Type)) throw new Exception(s"Generic type inputs were different; ${x.varType} did not equal ${template1Type}");
        })
        val retTypeTemplated = retType match {
          case Type.T1() | Type.Array(Type.T1()) if(template1Type != Type.Undefined()) => template1Type
          case _ => retType
        }
        ret += s"call ${fNameSignature(name, argTypes.map(x=>x.varType))}\n"
        ret += s"mov ${reg.head}, rax\n"
        ret += usedReg.reverse.map(x=>s"pop $x\n").mkString
        (ret, retTypeTemplated)
      }
      case None => {println(functions.find(x=>x.name == name).map(x=>x.args).mkString);throw new Exception(s"no overload of function $name matches argument list $argInputTypes")};
    }
  }
  //Maybe remove type assingmed after fact(causes issues when type is unknown in compile time)
  def setval(name: String, raxType: Type, env: Env): (String, Env) = {
    val look = lookup(name, env);
    var newenv = env;
    (look._2.varType, raxType) match {
      case (Type.Undefined(), Type.Undefined()) => {}
      case (Type.Undefined(), ass) => newenv = env.map(x=>if(x._1==name) (x._1, Variable(x._2.pointer, raxType)) else x)
      case (x,y) if x == y => {}
      case (x,y) => throw new Exception(s"trying to set variable of type ${look._2.varType} to $raxType")
    }

    (s"mov qword ${look._1}, rax\n", newenv)
  }
  //TODO add runtime index checking
  /*
  def setArray(name: String, index: Expr, raxType: Type, env: Env): (String, Env) = (lookup(name, env), convert(index, defaultReg, env)) match {
    case ((varLoc, Variable(loc, Type.Array(elemType))), (indexCode, indexType)) => {
      var newenv = env;
      if(elemType != raxType) {
        if(elemType == Type.Undefined()) newenv = env.map(x=>if(x._1==name) (x._1, Variable(x._2.pointer, Type.Array(raxType))) else x)
        else throw new Exception(s"trying to set array element of type ${elemType} to $raxType")
      }
      if(indexType != Type.Num()) throw new Exception(s"wrong index for array, got $indexType")
      ("push rax\n" + indexCode + s"mov rdi, ${varLoc}\n" + "pop rsi\n" + s"mov [rdi+8+rax*8], rsi\n", newenv)
    }
  }

   */
  def setArray(arr: Expr, index: Expr, raxType: Type, env: Env): String = (convert(arr, defaultReg, env), convert(index, defaultReg, env)) match {
    case ((arrCode, Type.Array(elemType)), (indexCode, indexType)) => {
      if(elemType != raxType) throw new Exception(s"trying to set array element of type ${elemType} to $raxType")
      if(indexType != Type.Num()) throw new Exception(s"wrong index for array, got $indexType")
      "push rax\n" + arrCode + "push rax\n" + indexCode + "pop rdi\n" + "pop rsi\n" + s"mov [rdi+8+rax*8], rsi\n"
    }
  }
  def getArray(arr: Expr, index: Expr, reg: List[String], env: Env): (String, Type) = (convert(arr, reg, env), convert(index, reg, env)) match {
    case ((code, Type.Array( arrType)), (indexCode, indexType)) => {
      if(indexType != Type.Num()) throw new Exception(s"wrong index for array, got $indexType")
      val size = arraySizeFromType(arrType);
      (code + s"push ${reg.head}\n" + indexCode + s"mov ${reg.tail.head}, ${reg.head}\n" +
        s"pop ${reg.head}\n" + s"mov ${sizeToReg(size, reg.head)}, [${reg.head}+8+${reg.tail.head}*$size]\n", arrType)
    }
    case ((code, varType), l) => throw new Exception(s"trying to access variable ${arr} as an array, has type $varType")
  }
  def getArraySize(arr: Expr, reg: List[String], env: Env): (String, Type) = convert(arr, reg, env) match {
    case (code, Type.Array(arrType)) => {
      (code + getArrayDirect(reg.head, 0, 8, reg), Type.Num())
    }
    case (code, varType) => throw new Exception(s"trying to access variable ${arr} as an array, has type $varType")
  }
  //TODO not safe when default values use rdi
  def setArrayDirect(code: String, index: Int, size: Int): String = {
    s"mov rdi, ${code}\n" + s"mov [rdi+${index*size}], ${sizeToReg(size, "rax")}\n"
  }
  def getArrayDirect(code: String, index: Int, size: Int, reg: List[String]): String = {
    s"mov ${reg.tail.head}, ${code}\n" + s"mov ${reg.head}, [${reg.tail.head}+${index*size}]\n"
  }
  //List("rax", "rdi", "rsi", "rdx", "rcx", "r8", "r9", "r10", "r11")
  val fullToByteReg: Map[String, String] = Map(("rax", "al"), ("rdi", "dil"), ("rsi", "sil"), ("rdx","dl"))
  def sizeToReg(size: Int, reg: String): String = size match {
    case 8 => reg
    case 1 => fullToByteReg.getOrElse(reg, reg)
  }
  def arraySizeFromType(valtype: Type): Int = valtype match {
    case Type.Undefined() => 8
    case Type.Character() => 1
    case Type.Num() => 8
    case Type.NumFloat() => 8
    case Type.Function(_,_) => 8
    case Type.T1() => 8
  }

  def defineArrayKnown(size: Int, setElemType:Type, defaultValues: List[Expr], env: Env): (String, Type) = {
    defineArray(s"mov rax, 0${size}d\n", setElemType, defaultValues, env)
  }
  //TODO use available registers or save, not rax
  def defineArray(size: String, setElemType:Type, defaultValues: List[Expr], env: Env): (String, Type) = {
    var elemType: Type = setElemType;
    var ret = defaultValues.zipWithIndex.map{case (entry, index) => {
      val converted = convert(entry, defaultReg, env)
      if(elemType == Type.Undefined()) elemType = converted._2;
      else if(converted._2 != elemType) throw new Exception(s"array elements are of different types")
      converted._1 + setArrayDirect("[rsp]", skipArrSize(index, arraySizeFromType(elemType)), arraySizeFromType(elemType));
    }}.mkString;
    val array_elem_size = arraySizeFromType(elemType);
    val array_def = size + "push rax\n" + "add rax, 1\n" + s"mov rdi, rax\n" + s"mov rsi, ${array_elem_size}\n" + "call calloc\n" + "push rax\n"
    ret = array_def + ret;
    ret += "pop r9\npop rax\n" + setArrayDirect("[rsp-16]", 0, 8)
    ret += "mov rax, r9\n"
    (ret, Type.Array(elemType))
  }
  def skipArrSize(index: Int, size: Int): Int = index + (8/size)
  /*
  def defineString(value: String, reg: List[String]): String = {
    //TODO Make definitions dynamic also
    //var ret = s"mov rdi, ${value.length+1}\n" + s"mov rsi, 8\n" + "call calloc\n" + "push rax\n" + "mov r9, rax\n";
    val label = s"string_${stringLiterals.length}";
    stringLiterals = stringLiterals :+ s"$label:\n        db  \"${value}\", 10, 0\n"
    s"mov ${reg.head}, $label\n"
  }
   */

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

  def floatTemplate (codeLeft: String, codeRight: String, command: String, reg: List[String]): (String, Type) = {
    val ret = codeLeft + codeRight + s"movq xmm0, ${reg.head}\n" + s"movq xmm1, ${reg.tail.head}\n" +
      s"${command} xmm0, xmm1\n" + s"movq ${reg.head}, xmm0\n"
    (ret, Type.NumFloat());
  }

  def binOpTemplate(left: Expr, right: Expr, command: String, reg: List[String], env: Env): String = {
    val leftout = convert(left, reg, env)._1;
    val rightout = convert(right, reg.tail, env)._1;
    leftout + rightout + s"${command} ${reg.head}, ${reg.tail.head}\n";
  }
  /*
  def mulTemplate(left: Expr, right: Expr, command: String, reg: List[String], env: Env): String = {
    val leftout = convert(left, reg, env)._1;
    val rightout = convert(right, reg.tail, env)._1;
    leftout + rightout + s"${command} ${reg.tail.head}\n";
  }
   */
  def intBinOpTemplate(codeLeft: String, codeRight: String, command: String, reg: List[String]): (String, Type) = {
    (codeLeft + codeRight + s"${command} ${reg.head}, ${reg.tail.head}\n", Type.Num());
  }
  def intmulTemplate(codeLeft: String, codeRight: String, command: String, reg: List[String]): (String, Type) = {
    (codeLeft + codeRight + s"${command} ${reg.tail.head}\n", Type.Num());
  }
  def aritTemplate(left: Expr, right: Expr, commandInt: String, commandFloat: String, reg: List[String], env: Env): (String, Type) = {
    (convert(left, reg, env), convert(right, reg.tail, env)) match {
      case ((codeLeft, Type.Num()), (codeRight, Type.Num())) =>
        if(commandInt == "add" || commandInt == "sub") intBinOpTemplate(codeLeft, codeRight, commandInt, reg)
        else intmulTemplate(codeLeft, codeRight, commandInt, reg)
      case ((codeLeft, Type.NumFloat()), (codeRight, Type.NumFloat())) => floatTemplate(codeLeft, codeRight, commandFloat, reg)
      case ((codeLeft, Type.Num()), (codeRight, Type.NumFloat())) => floatTemplate(codeLeft + convertToFloat(reg.head), codeRight, commandFloat, reg)
      case ((codeLeft, Type.NumFloat()), (codeRight, Type.Num())) => floatTemplate(codeLeft, codeRight + convertToFloat(reg.tail.head), commandFloat, reg)
      case ((codeLeft, typeLeft), (codeRight, typeRight)) => throw new Exception(s"can't perform arithmetic on operands of types ${typeLeft} and ${typeRight}");
    }
  }
  def convertToFloat(reg: String): String = {
    s"cvtsi2sd xmm0, ${reg}\n" + s"movq ${reg}, xmm0\n"
  }
  def convertToInt(reg: String): String = {
    s"movq xmm0, ${reg}\n" + s"cvtsd2si ${reg}, xmm0\n"
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
    ifCounter+=1
    converted._2 match {
      case Type.Num() => converted._1 + printTemplate("format_num");
      case Type.NumFloat() => converted._1 + "movq xmm0, rax\n" + "mov rdi, format_float\n" + "mov rax, 1\n" + "call printf\n"
      //case (Type.Str) => converted._1 + printTemplate("format_string");
      case Type.Bool() => converted._1 + s"cmp rax, 0\nje bool_${ifCounter}\n" + printTemplate("format_true") +
        s"jmp boole_${ifCounter}\nbool_${ifCounter}:\n" + printTemplate("format_false") + s"boole_${ifCounter}:\n";
      case Type.Character() => converted._1 + printTemplate("format_char");
      case Type.Array(Type.Character()) => converted._1 + "add rax, 8\n" + printTemplate("format_string");
      case _ => throw new Exception(s"input of type ${converted._2} not recognized in print")
    }
  }
  //TODO runtime memory garbage collection, currently only pointers on the stack are handled
  def freeMemory(env: Env): String = env.foldLeft("")((acc, entry) => entry._2.varType match {
    case Type.Array(arrType) => acc + s"mov rdi, [rbp-${entry._2.pointer}]\n" + "call free\n";
    //case Type.Interface(args) => acc + s"mov rdi, [rbp-${entry._2.pointer}]\n" + "call free\n";
    case _ => acc
  })

  type Env = Map[String, Variable]
  case class FunctionInfo(name: String, args: List[InputVar], retType: Type)
  //case class InterfaceInfo(name: String, args: List[InputVar])
  case class InterfaceInfo(name: String, args: List[InputVar], funcs: List[FunctionInfo])
  case class EnumInfo(name: String, el: List[String])
  case class Variable(pointer: Int, varType: Type)
}

