package posharp

import posharp.ToAssembly.{FunctionInfo, InterfaceInfo, EnumInfo, Variable, FileDeclaration}
import posharp.Type.{UserType, defaultValue, shortS, toLLVM}
import sourcecode.Text.generate

import scala.io.AnsiColor
import scala.language.postfixOps
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.termNames

class ToAssembly(currentFile: String) {
  type Env = Map[String, Variable]


  var varc: Counter = new Counter();
  var ifCounter = 0;
  var subconditionCounter: Int = 0;
  var stringLiterals: List[String] = List()
  var functions: List[FunctionInfo] = List();
  var interfaces: List[InterfaceInfo] = List();
  var enums: List[EnumInfo] = List()
  var lambdas: List[(Expr.Func, Env)] = List()
  var templateFunctions: List[Expr.Func] = List()
  var templateFunctionInstances: List[Expr.Func] = List()
  var functionScope: FunctionInfo = FunctionInfo("main", "", List(), Type.Num(), List());
  var templateInterfaces: List[Expr.DefineInterface] = List()
  var templateInterfaceInstances: List[Expr.DefineInterface] = List()
  val file_prefix: String = formatFName(currentFile)

  //var otherFilesGenerate: Map[String, (FileDeclaration, List[Expr.Func])] = Map()

  def declarationPass(input: Expr): Unit = {
    input match {
      case x: Expr.TopLevel => {
        declareFunctions(x);
        declareInterfaces(x)
        //declareEnums(x)
        templateInterfaces = x.interfaces.filter(x => isTemplateInterface(x.templates))
        templateFunctions = x.functions.filter(x => isTemplateFunction(x))
      }
    }
  }
  def convertMain(input: Expr, otherFiles: Map[String, Expr.TopLevel]): String = {
    ifCounter = 0;
    subconditionCounter = 0;
    functionScope = FunctionInfo("main", "", List(), Type.Num(), List());

    var converted =
      """
        | target triple = "x86_64-pc-linux-gnu"
        | target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
        |
        | declare i32 @printf(i8*, ...)
        | declare i64* @calloc(i32, i32)
        | declare ptr @malloc(i32)
        | declare void @free(ptr)
        | declare ptr @memcpy(ptr, ptr, i32)
        | declare void @setbuf(ptr noundef, ptr noundef)
        | declare void @exit(i32)
        | @format_num = private constant [3 x i8] c"%d\00", align 16
        | @format_uint = private constant [4 x i8] c"%lu\00", align 16
        | @format_float = private constant [3 x i8] c"%f\00", align 16
        | @format_string = private constant [3 x i8] c"%s\00", align 16
        | @format_char = private constant [3 x i8] c"%c\00", align 16
        | @format_false = private constant [7 x i8] c"false\0A\00", align 16
        | @format_true = private constant [7 x i8] c"true\0A\00\00", align 16
        | @stdout = external global ptr, align 8
        | %Type.array.double = type {i32, double*}
        | %Type.array.i32 = type {i32, i32*}
        | %Type.array.i8 = type {i32, i8*}
        | %Type.array.i1 = type {i32, i1*}
        |
        |""".stripMargin;
    input match { case x: Expr.TopLevel => {
      val interfaceFunctionList: List[(Expr.Func, Env)] = x.interfaces.filter(x=> !isTemplateInterface(x.templates)).flatMap(intf =>
          addPrefixToFunctions(intf.name, intf.functions)
        ).filter(y=>y.templates.isEmpty)
        .map(y => (y, Map()))
      //TODO template function in interfaces ignored for now

      converted += declareInterfaces(x, onlyLLVM = true)
      converted += exportDeclarations(currentFile) + "\n"
      converted += handleImports(x, otherFiles) + "\n"
      converted += defineFunctions(x.functions.map(y=>(y, Map())), false, false);
      converted += defineFunctions(interfaceFunctionList, false, false);
    }}
    converted += defineFunctions(templateFunctionInstances.map(x=>(x, Map())), false, true)
    converted += templateInterfaceInstances.map(intf => {
      val llvm_intf = Type.toLLVM(Type.Interface(intf.name, intf.props, List(), intf.templates)).dropRight(1)
      val types = intf.props.map(x => Type.toLLVM(x.varType)).mkString(", ")
      s"$llvm_intf = type {$types}\n"
    }).mkString
    //converted += defineFunctions(lambdas, true);
    converted += stringLiterals.mkString
    converted += """attributes #0 = { mustprogress noinline nounwind optnone uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
                   |attributes #1 = { nocallback nofree nosync nounwind readnone speculatable willreturn }
                   |attributes #2 = { "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }""".stripMargin
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
          case Some(InterfaceInfo(itfName, props, funcs, templates)) => ("", Type.Interface(itfName, props, funcs, templates))
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
      case Expr.DefineArray(size, elemType, defaultValues) => convertLoc(size, env) match {
        case (code, Type.Num(), loc) => {
          val r = defineArray(loc, elemType, defaultValues, env)
          (code + r._1, r._2)
        }
        case (_, x, _) => throw new Exception(s"not number when defining array size, got input of type $x")
      }
      case Expr.GetArray(name, index) => getArray(name, index, env);
      case Expr.ArraySize(name) => getArraySize(name, env);

      case Expr.GetProperty(obj, prop) => convertLoc(obj, env) match {
        case(code, intf_type@Type.Interface(name, props, funcs, templates), loc) => props.find(x=>x.name == prop) match {
          case Some(n) => {
            val intfDec = Type.toLLVM(intf_type)
            val idx = props.indexOf(n);
            //TODO i32 0,
            val ret = code + s"${varc.next()} = getelementptr inbounds $intfDec, $intfDec* ${varc.secondLast()}, i32 $idx\n" +
             s"${varc.next()} = load ${Type.toLLVM(n.varType)}, ${Type.toLLVM(n.varType)}* ${varc.secondLast()}, align 8\n"
            (ret, n.varType)
          }
          case None => throw new Exception(s"interface ${interfaces.find(x=>x.args == props).get.name} does not have a property ${prop}")
        }
        /*
        //case (code, Type.StaticInterface(props, funcs)) =>
        case (code, Type.Enum(el)) => (s"mov ${reg.head}, 0${el.indexOf(prop)}d\n", Type.Num())
         */
        case (code, Type.Array(a), loc) if prop == "size" => getArraySize(Expr.Compiled(code, Type.Array(a), loc), env)//conv(Expr.ArraySize(obj))
        case (_, valType, _) => throw new Exception(s"expected a interface, got ${valType}")
      }
      case Expr.CallF(name, args, templates) => {
        if(functions.exists(x=>x.name == name)) interpFunction(name, args, templates, env)
        //else if(env.contains(name)) callLambda(Expr.Ident(name), args, reg, env)
        else throw new Exception(s"unknown identifier $name")
      }
      case Expr.CallObjFunc(obj, func) => convertLoc(obj, env) match {
        //TODO investigate callobj
        //case _ => throw new NotImplementedError("")
        case (code, t@Type.Interface(_, props, funcs, templates), loc) => callObjFunction(Expr.Compiled(code, t, loc), func, props, funcs, isStatic = false, env)
        case (code, t@Type.StaticInterface(props, funcs), loc) => callObjFunction(Expr.Compiled(code, t, loc), func, props, funcs, isStatic = true, env)
      }
      case Expr.InstantiateInterface(name, values, templates) => {
        if (templates.nonEmpty) interfaces.find(x=>x.name == name && x.templates == templates) match {
          case Some(intf) => {}
          case None => interfaces.find(x=>x.name == name && isTemplateInterface(x)) match {
            case Some(intf) => {
              //val classT = Type.toLLVM(Type.Interface(intf.name, intf.args, intf.funcs, templates)) // s"%Class.$name"

              //TODO full error message
              val template_mappings = intf.templates.zip(templates)
              template_mappings.groupBy(x => x._1).filter(x => Set(x._2).toList.length > 1).foreach(x => throw new Exception(s"Template interface input types conflicting"))
              //println(template_mappings)
              val replace_func = replaceWithMappingFunc(template_mappings)

              val intf_expr = templateInterfaces.find(x => x.name == name && intf.templates == x.templates).get
              //func_expr = Expr.Func(func_expr.name,func_expr.argNames,func_expr.retType,func_expr.body,func_expr.templates)
              //templateFunctionInstances = templateFunctionInstances :+ replaceType(func_expr, replace_func).asInstanceOf[Expr.Func]
              val new_intf_expr = replaceType(intf_expr, replace_func).asInstanceOf[Expr.DefineInterface]
              templateInterfaceInstances = templateInterfaceInstances :+ new_intf_expr
              val newInterfaceInfo = traverseTypeTree(intf, replace_func)
              interfaces = interfaces :+ newInterfaceInfo

              functions = functions ::: addPrefixToFunctions(intf.name, newInterfaceInfo.funcs)
              templateFunctionInstances = templateFunctionInstances ::: addPrefixToFunctions(intf.name, new_intf_expr.functions)
            }
            case None => throw new Exception(s"no interface with name \"$name\" defined")
          }
        }
        interfaces.find(x=>x.name == name && x.templates == templates) match {
          case Some(intf) => {
            val classT = toLLVM(intf) // s"%Class.$name"

            var aloc = "";
            aloc += s"${varc.next()} = getelementptr $classT, $classT* null, i32 1\n" + s"${varc.next()} = ptrtoint $classT** ${varc.secondLast()} to i32\n"
            val bytesLoc = varc.last();
            aloc += s"${varc.next()} = call ptr (i32) @malloc(i32 $bytesLoc)\n";
            val alocLoc = varc.last();

            val valuesCompiled = values.map(x => convertLoc(x, env)).map(x => Expr.Compiled(x._1, x._2, x._3))
            intf.funcs.find(x => x.name == name && x.args == values)
            val func_code = interpFunction(name + "_" + name, Expr.Compiled(aloc, UserType(name, templates), alocLoc) +: valuesCompiled, List(), env)._1
            (func_code, Type.Interface(name, intf.args, intf.funcs, intf.templates))
          }
          case None => throw new Exception(s"no interface with name \"$name\" defined")
        }
      }
      case Expr.Str(value) => (defineString(value), Type.Str())
      case Expr.Character(value) => (s"${varc.next()} = add i8 0, ${value.toInt}\n", Type.Character())
      case Expr.Convert(value, valType: Type) => (convertLoc(value, env), valType) match {
        case ((code, Type.Num(), loc), Type.NumFloat()) => (code + s"${varc.next()} = sitofp ${Type.toLLVM(Type.Num())} $loc to ${Type.toLLVM(Type.NumFloat())}\n", valType)
        case ((code, Type.NumFloat(), loc), Type.Num()) => (code + s"${varc.next()} = fptosi ${Type.toLLVM(Type.NumFloat())} $loc to ${Type.toLLVM(Type.Num())}\n", valType)
        case ((code, Type.Num(), loc), Type.Character()) => (code + s"${varc.next()} = sext ${Type.toLLVM(Type.Num())} $loc to ${Type.toLLVM(Type.Character())}\n", valType)
        case ((code, Type.Character(), loc), Type.Num()) => (code + s"${varc.next()} = trunc ${Type.toLLVM(Type.Character())} $loc to ${Type.toLLVM(Type.Num())}\n", valType)
        case ((code, l, _), r) => throw new Exception(s"cant convert from type ${l} to type $r")
      }
      /*


      //
      case Expr.Str(value) => (defineArrayKnown(value.length, Type.Character(), value.map(x=>Expr.Character(x)).toList, env)._1, Type.Array(Type.Character()))


      case Expr.Lambda(args, ret, body) => {
        val label = "lambda_" + lambdas.size
        functions = functions :+ FunctionInfo(label, args, ret)
        lambdas = lambdas :+ (Expr.Func(label, args, ret, body), env)
        (s"mov ${reg.head}, $label\n", Type.Function(args.map(x=>x.varType), ret))
      }
       */
      case Expr.Nothing() => ("", Type.Undefined());
      case Expr.Compiled(code, retType, _) => (code, retType);
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
        val look = lookupLoc(name, env)
        val converted = convertLoc(value, env);
        if(makeUserTypesConcrete(look.varType) != converted._2) throw new Exception(s"mismatch when assigning value" +
          s" to variable $name, expected ${look.varType}, but got ${converted._2}")
        val set = s"store ${Type.toLLVM(converted._2)} ${converted._3}, ${Type.toLLVM(converted._2)}* ${look.loc}, align ${arraySizeFromType(converted._2)}\n"
        converted._1 + set;
      };
      case Expr.DefVal(name, varType) => {
        val loc = s"%$name.${varc.extra()}"
        newenv = newVar(name, loc, varType, newenv);
        s"$loc = alloca ${Type.toLLVM(varType)}\n" //, align ${arraySizeFromType(varType)}
      }
      case Expr.DefValWithValue(variable, varType, value) => {
        var newType = varType
        val converted = convertLoc(value, env);
        if(newType == Type.Undefined()) newType = converted._2;
        if(makeUserTypesConcrete(newType) != converted._2) throw new Exception(s"mismatch when assigning value" +
          s" to variable $variable, expected ${newType}, but got ${converted._2}")

        val loc = s"%$variable.${varc.extra()}"
        val set = s"store ${Type.toLLVM(converted._2)} ${converted._3}, ${Type.toLLVM(converted._2)}* $loc, align ${arraySizeFromType(converted._2)}\n";
        newenv = newVar(variable, loc, newType, newenv);
        s"$loc = alloca ${Type.toLLVM(newType)}, align 64\n" + converted._1 + set; //, align 4 ${arraySizeFromType(newType)}
      }
      case Expr.If(condition, ifTrue, ifFalse) => {
        def compare(left: Expr, right: Expr, numeric: Boolean): String =
          compareExpr(left, right, numeric, "eq", "oeq", env)
        val trueLabel = s"if_${ifCounter}_true"
        val falseLabel = s"if_${ifCounter}_false"
        val endLabel = s"if_${ifCounter}_end"
        ifCounter += 1;
        val cond = convertLoc(condition, env) match {
          case (code, Type.Bool(), loc) => compare(Expr.Compiled(code, Type.Bool(), loc), Expr.True(), false)
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
        case(code, intf_type@Type.Interface(name, props,f, templates), intfLoc) => props.find(x=>x.name == prop) match {
          case Some(n) => convertLoc(valueRaw, env) match {
            case (valCode, valType, valueLoc) if(valType == n.varType) => {
              val intfDec = Type.toLLVM(intf_type)//s"%Class.$name"
              val idx = props.indexOf(n);
              var ret = code + valCode
              //removed i32 0, not sure if that could cause issues
              ret += s"${varc.next()} = getelementptr $intfDec, $intfDec* $intfLoc, i32 $idx\n"
              ret += s"store ${Type.toLLVM(valType)} $valueLoc, ${Type.toLLVM(n.varType)}* ${varc.last()}, align ${arraySizeFromType(n.varType)}\n"
              ret
            }
            case (_, valType, _) => throw new Exception(s"trying to property ${n.name} of type ${n.varType} to incompatible type ${valType}")
          }
          case None => throw new Exception(s"interface ${interfaces.find(x=>x.args == props).get.name} does not have a property ${prop}")
        }
        case (_, valType, _) => throw new Exception(s"expected an interface, got ${valType}")
      }
      case Expr.ThrowException(err) => {
        val msg = AnsiColor.RED + "RuntimeException: " + err + AnsiColor.RESET + "\n"
        defineString(msg) + printTemplate("format_string", "i8*", varc.last()) + "call void @exit(i32 1)\n" // "br label %exception\n"
      }
      case x@Expr.CallObjFunc(obj, func) => convert(x, env)._1;
      case x@Expr.CallF(_,_,_) => convert(x, env)._1;
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
      }
      case Expr.Print(toPrint) => printInterp(toPrint, env);
      case Expr.Free(toFree) => convertLoc(toFree, env) match {
        case (code, _:Type.Array | _:Type.UserType | _:Type.Interface, loc) => {
          code + s"call void @free(ptr $loc)\n"
        }
      }
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
    functions = input.functions.map(x=> FunctionInfo(x.name, "", x.argNames, x.retType, x.templates))
  }
  private def declareInterfaces(input: Expr.TopLevel, onlyLLVM: Boolean = false): String = {
    val interfaces_local = input.interfaces.map(x=> InterfaceInfo(x.name, x.props, x.functions.map(x=>FunctionInfo(x.name,"", x.argNames,x.retType,x.templates)), x.templates))
    val non_generic_intf = interfaces_local.filter(x=> !isTemplateInterface(x))
    if (!onlyLLVM) {
      functions = functions ::: non_generic_intf.flatMap(x => addPrefixToFunctions(x.name, x.funcs))
      interfaces = interfaces_local
    }
    non_generic_intf.map(intf => {
      val types = intf.args.map(x=>Type.toLLVM(x.varType)).mkString(", ")
      s"%Class.${intf.name}. = type {$types}\n"
    }).mkString
  }
  //https://stackoverflow.com/questions/3307427/scala-double-definition-2-methods-have-the-same-type-erasure
  private def addPrefixToFunctions(prefix: String, funcs: => List[FunctionInfo]): List[FunctionInfo] =
    funcs.map(y=>FunctionInfo(prefix+"_"+y.name, y.prefix, y.args, y.retType,y.templates))

  private def addPrefixToFunctions(prefix: String, funcs: List[Expr.Func]): List[Expr.Func] = {
    funcs.map(func => Expr.Func(prefix + "_" + func.name, func.argNames, func.retType, func.body, func.templates))
  }
  private def declareEnums(input: Expr.TopLevel): Unit = {
    enums = input.enums.map(x=>EnumInfo(x.name,x.props))
  }
  private def handleImports(input: Expr.TopLevel, otherFiles: Map[String, Expr.TopLevel]): String = {
    input.imports.map(user_imp=>{
      val imp = Expr.Import(formatFName(user_imp.file)+"_"+user_imp.toImport, user_imp.file)
      def change_file(name: String) = name.replace(formatFName(imp.file)+"_", file_prefix+"_")
      if (!otherFiles.contains(imp.file)) throw new Exception(s"file \"${imp.file}\" could not be imported");
      val top = otherFiles(imp.file)
      var ret = ""

      val funcsForImport = searchFileDeclarations(top, imp) match {
        case f_def@Expr.Func(name, argnames, retType, code, templates) => {
          val info = FunctionInfo(name, formatFName(imp.file), argnames, retType, templates)
          functions = functions :+ info
          //TODO MAJOR issue with where generic classes will be generated. Should be in source file but currently in current.
          if (isTemplateFunction(info)) {
            templateFunctions = templateFunctions :+ f_def
            List()
          }
          else List(FunctionInfo(name, formatFName(imp.file), argnames, retType, templates))
        }
        case intf_def@Expr.DefineInterface(name, props, i_functions, templates) => {
          val intf = InterfaceInfo(name, props, i_functions.map(x=>FunctionInfo(x.name,formatFName(imp.file), x.argNames,x.retType,x.templates)), templates)

          //templateFunctions = x.functions.filter(x => isTemplateFunction(x))
          //TODO MAJOR issue with where generic classes will be generated. Should be in source file but currently in current.
          if(isTemplateInterface(intf)) templateInterfaces = templateInterfaces :+ intf_def

          interfaces = interfaces :+ intf
          val same_namespace_intf = InterfaceInfo(change_file(name), props, intf.funcs, templates)
          interfaces = interfaces :+ same_namespace_intf

          if(!isTemplateInterface(intf)) {
            val types = intf.args.map(x => Type.toLLVM(x.varType)).mkString(", ")
            val intf_llvm = toLLVM(intf).dropRight(1)
            val same_namespace_intf_llvm = toLLVM(same_namespace_intf).dropRight(1)
            //ret += s"%Class.${intf.name} = type {$types}\n"
            ret += s"$intf_llvm = type {$types}\n"
            ret += s"$same_namespace_intf_llvm = type {$types}\n"

            val funcs = addPrefixToFunctions(intf.name, intf.funcs)
            functions = functions ::: funcs
            funcs.map(x => FunctionInfo(x.name, formatFName(imp.file), x.args, x.retType, x.templates))
          } else List()
        }
      }
      funcsForImport.foreach(info=>{
        functions = functions :+ FunctionInfo(change_file(info.name), info.prefix, info.args, info.retType, info.templates)
      })
      ret + funcsForImport.map(info=>{
        val importName: String = fNameSignature(info)
        val name = change_file(importName) //formatFName(imp.file) + "_" +
        val args = info.args.map(x=>s"${Type.toLLVM(x.varType)}").mkString(", ")
        //s"declare ${Type.toLLVM(info.retType)} @${importName}($args)\n" +
        //  s"@$name = ifunc ${toLLVM(info)}, ${toLLVM(info)}* @${importName}\n"
        s"declare ${Type.toLLVM(info.retType)} @${importName}($args) \"${formatFName(imp.file)}\"\n"+
          s"@${name} = alias ${toLLVM(info)}, ${toLLVM(info)}* @${importName}\n"

      }).mkString
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
      .filter(x=> ! isTemplateFunction(x))
      .map(info => {
        val formatFile = formatFName(file)
        val name = fNameSignature(info)
        s"@${formatFile}_${name} = external alias ${toLLVM(info)}, ${toLLVM(info)}* @${name}\n"
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
    case UserType(name, templates) => interfaces.find(x=>x.name == name && x.templates == templates) match {
      case Some(n) => Type.Interface(name, n.args, n.funcs, n.templates)
      case _ => throw new Exception (s"no interface of name $name");
    }
    case x => x
  }

  def fNameSignature(info: FunctionInfo): String = fNameSignature(info.name, info.args.map(x=>x.varType))
  def fNameSignature(name: String, args: List[Type]):String = name + (if(args.isEmpty) "" else ".") + args.map(x=>shortS(x)).mkString
  def toLLVM(intf: InterfaceInfo): String = Type.toLLVM(Type.Interface(intf.name, intf.args, intf.funcs, intf.templates))
  def toLLVM(info: FunctionInfo): String = {
    val args = info.args.map(x=>s"${Type.toLLVM(x.varType)}").mkString(", ")
    s"${Type.toLLVM(info.retType)} ($args)"
  }

  private def defineFunctions(input: List[(Expr.Func, Env)], lambdaMode: Boolean, templated: Boolean): String = {
    input.map{ case (function, upperScope) => {
      if(isTemplateFunction(function) && !templated) {
        ""
      }
      else {
        //print(Util.prettyPrint(functions))
        //print(Util.prettyPrint(function))
        val info = functions.find(x => x.name == function.name && x.args == function.argNames).get;
        functionScope = info;

        val fname = fNameSignature(info)
        val args = info.args.map(x => s"${Type.toLLVM(x.varType)} %Input.${x.name}").mkString(", ")
        val addPrivate = if (info.name == "main") "" else "private ";
        var ret = s"define $addPrivate${Type.toLLVM(info.retType)} @${fname}($args) #0 {\n"
        val newEnv = upperScope ++ info.args.map(x => (x.name, Variable(s"%${x.name}", x.varType))).toMap //Variable(s"%${x.name}.${varc.extra()}"
        var body = info.args.map(x =>
          s"%${x.name} = alloca ${Type.toLLVM(x.varType)}, align 64\n" + //, align ${arraySizeFromType(x.varType)}
            s"store ${Type.toLLVM(x.varType)} %Input.${x.name}, ${Type.toLLVM(x.varType)}* %${x.name}\n").mkString //, align ${arraySizeFromType(x.varType)}
        //TODO might want to handle name shadowing here

        body += "%dummy = alloca i32\n"
        body += convert(function.body, newEnv)._1

        if (info.retType == Type.Undefined()) body += "ret void\n"
        if (info.name == "main") body += "ret i32 0\n"
        varc.reset()

        ret += body.split("\n").map(x => "\t" + x).mkString("\n")
        ret += "\n}\n"
        ret
      }
    }}.mkString
  }
  def interpFunction(name: String, args: List[Expr], templates: List[Type], env: Env ): (String, Type) = {
    val argRet = args.map(arg => convertLoc(arg, env))
    val argInputTypes = argRet.map(x => makeUserTypesConcrete(x._2))
    var ret = argRet.map(x=>x._1).mkString
    val argsString = argRet.map(x=>s"${Type.toLLVM(x._2)} ${x._3}").mkString(", ")

    functions.find(x=>x.name == name) match {
      case Some(x) => ; case None => throw new Exception(s"function of name $name undefined");
    }
    if (!functions.exists(x=>x.name == name && argInputTypes == x.args.map(x=>makeUserTypesConcrete(x.varType)))) {
      functions.find(x => x.name == name && argInputTypes.length == x.args.length && isTemplateFunction(x)) match {
        case Some(info@FunctionInfo(p, prefix, argTypes, retType, _)) => {

          if (info.templates.length != templates.length) throw new Exception(s"Wrong template count for calling function $name")
          val template_mappings = info.templates.zip(templates) //templateFunctionArgs(info, templates)

          template_mappings.groupBy(x => x._1).filter(x => Set(x._2).toList.length > 1)
            .foreach(x => throw new Exception(s"Template function $name input types conflicting, received: ${x._2}"))
          //println(template_mappings)
          val replace_func = replaceWithMappingFunc(template_mappings)

          val func_expr = templateFunctions.find(x => x.name == name && argTypes == x.argNames).get
          //func_expr = Expr.Func(func_expr.name,func_expr.argNames,func_expr.retType,func_expr.body,func_expr.templates)
          templateFunctionInstances = templateFunctionInstances :+ replaceType(func_expr, replace_func).asInstanceOf[Expr.Func]
          //TODO unsure if templates or input.templates is best
          functions = functions :+ FunctionInfo(p, prefix, argTypes.map(x => InputVar(x.name, traverseTypeTree(x.varType, replace_func))), traverseTypeTree(retType, replace_func), info.templates)
        }
        case _ => ()
      }
    }

    //functions.find(x=>x.name == name && Type.compare(argInputTypes, x.args.map(x=>makeUserTypesConcrete(x.varType)))) match {
    println(Util.prettyPrint(functions))

    functions.find(x=>x.name == name && argInputTypes == x.args.map(x=>makeUserTypesConcrete(x.varType))) match {
      case Some(info@FunctionInfo(p, prefix, argTypes, retType, templates)) => {
        var tName = fNameSignature(info)
        //if(prefix != "") tName = prefix+"_"+tName;
        val argTypeString = argTypes.map(x=>Type.toLLVM(x.varType)).mkString(", ")
        if (retType != Type.Undefined()) ret += s"${varc.next()} = ";
        ret += s"call ${Type.toLLVM(retType)} ($argTypeString) @$tName($argsString)\n"
        (ret, retType)
      }
      case None => {
        //println(Util.prettyPrint(functions.find(x=>x.name == name).map(x=>x.args.map(y=>InputVar(y.name,makeUserTypesConcrete(y.varType)))).get));
        println(Util.prettyPrint(argInputTypes.last));
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
        interpFunction(intfName+"_"+func.name, args, func.templates, env)
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
  */
  def defineString(value: String): String = {
    val label = s"string.${stringLiterals.length}";
    val n = value.length + 1;
    var toString = value.replace("\n", "\\0A") + "\\00"
    toString = toString.replace("\u0000", "\\00")

    stringLiterals = stringLiterals :+ s"@$label = internal constant [$n x i8] c\"$toString\", align 16\n" //
    //s"${varc.next()} = getelementptr inbounds i8*, i8** @$label, i32 0, i32 0"
    //s"${varc.next()} = alloca i8*, align 8\n" +
      s"${varc.next()} = bitcast [$n x i8]* @$label to ptr\n"
      //s"store i8* @$label, i8** ${varc.last()}\n" +
      //s"${varc.next()} = load i8*, i8** ${varc.secondLast()}\n"
  }

  def getArrayPointerIndex(arrType: Type, arrLoc: String, indexLoc: String): String = {
    val Tsig = Type.toLLVM(arrType)
    val arrTC = s"%Type.array.$Tsig"

    var ret = "";
    ret += s"${varc.next()} = getelementptr $arrTC, ptr $arrLoc, i32 0, i32 1\n"
    val arrStructLoc = varc.last()
    ret += s"${varc.next()} = load ptr, ptr ${arrStructLoc}, align 8\n"
    ret += s"${varc.next()} = getelementptr inbounds $Tsig, ptr ${varc.secondLast()}, i32 $indexLoc\n"

    ret
  }

  def getArray(arr: Expr, index: Expr, env: Env): (String, Type) = (convertLoc(arr, env), convertLoc(index, env)) match {
    case ((code, Type.Array(arrType), arrLoc), (indexCode, indexType, indexLoc)) => {
      if(indexType != Type.Num()) throw new Exception(s"wrong index for array, got $indexType");
      val Tsig = Type.toLLVM(arrType)

      var ret = code + indexCode;
      ret += getArrayPointerIndex(arrType, arrLoc, indexLoc)
      ret += s"${varc.next()} = load $Tsig, $Tsig* ${varc.secondLast()}\n" //${arraySizeFromType(arrType)}
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

        ret += s"store $Tsig $valLoc, $Tsig* ${varc.last()}, align ${arraySizeFromType(arrType)}\n"
        ret
    }
  }
  def getArraySize(arr: Expr, env: Env): (String, Type) = convertLoc(arr, env) match {
    case (code, Type.Array(arrType), loc) => {
      val Tsig = "i32"
      val arrTC = s"%Type.array.$Tsig"

      val ret = code +
        s"${varc.next()} = getelementptr $arrTC, $arrTC* $loc, i32 0, i32 0\n" +
        s"${varc.next()} = load $Tsig, $Tsig* ${varc.secondLast()}, align 4\n"
      (ret, Type.Num())
    }
    case (_, varType, _) => throw new Exception(s"trying to access variable ${arr} as an array, has type $varType")
  }

  def defineArray(sizeLoc: String, setElemType:Type, defaultValues: List[Expr], env: Env): (String, Type) = {
    var elemType: Type = setElemType;
    val valuesConverted = defaultValues.map(entry => {
      val converted = convertLoc(entry, env)
      if(elemType == Type.Undefined()) elemType = converted._2;
      else if(converted._2 != elemType) throw new Exception(s"array elements are of different types")
      converted
    })

    val array_elem_size = arraySizeFromType(elemType);
    val Tsig = Type.toLLVM(elemType)
    val arrTC = s"%Type.array.$Tsig"
    var ret = "";
    ret += valuesConverted.map(x=>x._1).mkString
    ret += s"${varc.next()} = call i64* (i32, i32) @calloc(i32 $sizeLoc, i32 $array_elem_size)\n";
    ret += s"${varc.next()} = bitcast i64* ${varc.secondLast()} to $Tsig*\n"
    val arrLoc = varc.last()
    val sizeStructPointer = s"%arr.size.${varc.extra()}"
    val arrStructPointer = s"%arr.arr.${varc.extra()}"
    ret += s"${varc.next()} = alloca $arrTC\n" //changing to 8 seems to cause fault
    val structLoc = varc.last()

    ret += s"${sizeStructPointer} = getelementptr $arrTC, $arrTC* $structLoc, i32 0, i32 0\n"
    ret += s"store i32 $sizeLoc, i32* ${sizeStructPointer}, align 4\n"
    ret += s"${arrStructPointer} = getelementptr $arrTC, $arrTC* $structLoc, i32 0, i32 1\n"
    ret += s"store $Tsig* $arrLoc, $Tsig** ${arrStructPointer}, align 8\n"

    ret += valuesConverted.zipWithIndex.map{case ((code, retTy, loc), index) => {
      setArray(Expr.Compiled("", Type.Array(elemType), structLoc), Expr.Num(index), Expr.Compiled("", retTy, loc), env)
    }}.mkString;

    //ret += s"${varc.next()} = getelementptr $arrTC, $arrTC* null, i32 1\n" + s"${varc.next()} = ptrtoint $arrTC** ${varc.secondLast()} to i32\n"
    //ret += s"call i32 (ptr, ...) @printf(ptr @format_num, i32 ${varc.last()})\n"
    ret += s"${varc.next()} = add i32 8, 8\n"
    val bytesLoc = varc.last();
    ret += s"${varc.next()} = call ptr (i32) @malloc(i32 $bytesLoc)\n";
    val alocLoc = varc.last();
    ret += s"call ptr @memcpy(ptr $alocLoc, ptr $structLoc, i32 $bytesLoc)\n"

    ret += s"${varc.next()} = bitcast $arrTC* $alocLoc to $arrTC*\n"
    (ret, Type.Array(elemType))
  }
  def arraySizeFromType(valtype: Type): Int = valtype match {
    case Type.Undefined() => 8
    case Type.Character() => 1
    case Type.Num() => 4
    case Type.NumFloat() => 8
    case Type.Function(_,_) => 8
    case Type.T(_) => 8
    case _ => 8
  }

  def lookup(tofind: String, env: Env): (String, Variable) = {
    val ret = lookupLoc(tofind, env)
    (s"${varc.next()} = load ${Type.toLLVM(ret.varType)}, ${Type.toLLVM(ret.varType)}* ${ret.loc}, align ${arraySizeFromType(ret.varType)}\n", ret)
  }
  def lookupLoc(tofind: String, env: Env): Variable = env.get(tofind) match {
    case Some(v) => v
    case None => throw new Exception(s"variable \"${tofind}\" undefined")
  }
  def newVar(name: String, loc: String, varType: Type, env: Env) : Env = {
    if(env.contains(name)) throw new Exception(s"variable \"${name}\" already defined")
    env + (name -> Variable(loc, varType))
  }

  def convertLoc(input: Expr, env: Env): (String, Type, String) = {
    input match {
      case Expr.Compiled(code, ret, loc) => (code, ret, loc)
      case _ => {
        val ret = convert(input, env)
        (ret._1, ret._2, varc.last())
      }
    }

  }
  def convertType(input: Expr, env: Env): Type = {
    varc.pause();
    val c = varc.counter
    val ret = convert(input, env)._2
    varc.unpause();
    varc.counter = c;
    ret
  }


  def isTemplateFunction(input: Expr.Func): Boolean = {
    //templateFunctionArgs(input).nonEmpty
    input.templates.nonEmpty
  }
  def isTemplateInterface(input: InterfaceInfo): Boolean = {
    isTemplateInterface(input.templates)
  }
  def isTemplateInterface(templates: List[Type]): Boolean = {
    templates.exists {
      case Type.T(_) => true
      case _ => false
    }
  }
  def isTemplateFunction(input: FunctionInfo): Boolean = {
    input.templates.nonEmpty
  }

  def traverseTypeTree(input: FunctionInfo, func: (Type) => Type): FunctionInfo = {
    FunctionInfo(input.name,
      input.prefix,
      input.args.map(y =>
        InputVar(y.name, traverseTypeTree(y.varType, func))
        ),
      traverseTypeTree(input.retType, func),
      input.templates.map(y => traverseTypeTree(y, func).asInstanceOf[Type.T])
    )
  }
  def traverseTypeTree(input: InterfaceInfo, func: (Type) => Type): InterfaceInfo = {
    InterfaceInfo(input.name,
      input.args.map(y =>
        InputVar(y.name, traverseTypeTree(y.varType, func))
      ),
      input.funcs.map(y=> traverseTypeTree(y, func)),
      input.templates.map(y=>traverseTypeTree(y,func))
    )
  }
  def traverseTypeTree(input: Type, func: (Type) => Type): Type = input match {

    case Type.Interface(name, args,f, templates) => Type.Interface(name,
      args.map(x=>InputVar(x.name, traverseTypeTree(x.varType, func))),
      f.map(x=>traverseTypeTree(x, func)),
      templates.map(x=>traverseTypeTree(x, func))
    )
    case Type.Array(valType) => Type.Array(traverseTypeTree(valType, func))
    case Type.Function(args: List[Type], retType: Type) => Type.Function(args.map(x=>traverseTypeTree(x, func)), traverseTypeTree(retType, func))
    //StaticInterface(properties: List[InputVar], functions: List[FunctionInfo])
    case Type.UserType(name, templates) => Type.UserType(name, templates.map(x=>traverseTypeTree(x, func)))
    case x => func(x)
  }
  def replaceType(input: List[InputVar], func: (Type) => Type): List[InputVar] = {
    input.map(x =>
      InputVar(x.name, traverseTypeTree(x.varType, func)))
  }

  def replaceType(input: Expr, func: (Type) => Type): Expr = input match {
    case Expr.DefVal(a, varType) => Expr.DefVal(a, traverseTypeTree(varType, func))
    case Expr.DefValWithValue(variable, varType, value) => Expr.DefValWithValue(variable, traverseTypeTree(varType, func), replaceType(value, func))
    case Expr.Func(name, argNames, retType, body, templates) => Expr.Func(name, replaceType(argNames, func), traverseTypeTree(retType, func), replaceType(body, func).asInstanceOf[Expr.Block], templates)
    case Expr.Block(lines) => Expr.Block(lines.map(x=>replaceType(x, func)))
    case Expr.While(condition: Expr, execute: Expr.Block) => Expr.While(replaceType(condition, func), replaceType(execute,func).asInstanceOf[Expr.Block])
    case Expr.If(condition: Expr, ifTrue: Expr.Block, ifFalse: Expr) =>
      Expr.If(replaceType(condition, func), replaceType(ifTrue,func).asInstanceOf[Expr.Block],replaceType(ifFalse,func).asInstanceOf[Expr.Block])
    case Expr.DefineInterface(name, props, functions, templates) =>
      Expr.DefineInterface(name, replaceType(props, func), functions.map(x=>replaceType(x, func).asInstanceOf[Expr.Func]),
        templates.map(x=>traverseTypeTree(x, func))
      )
    //case Expr.DefineArray(size, elemType:Type, defaultValues: List[Expr]) => Expr.DefineArray(size, traverseTypeTree(elemType, func), defaultValues)
    //case Expr.InstantiateInterface
    case x => {
      val param_func: (Any => Any) = new ((Any) => Any) {
        def apply(in: Any): Any = in match {
          case expr: Expr => replaceType(expr, func)
          case t: Type => traverseTypeTree(t, func)
          case h :: t => (h +: t).map(y=>apply(y))
          case _ => in
        }
      }
      applyFunctionToUnknownCaseClass(x, param_func)
    }

      //case class Convert(value: Expr, to: Type) extends Expr
      //case class Lambda(argNames: List[InputVar], retType: Type, body: Expr.Block) extends Expr
  }

  def applyFunctionToUnknownCaseClass(instance: Expr, func: Any => Any): Expr = {
    val mirror = currentMirror
    val instanceMirror = mirror.reflect(instance)
    val classSymbol = instanceMirror.symbol

    if (!classSymbol.isClass || !classSymbol.asClass.isCaseClass) {
      throw new IllegalArgumentException("The provided instance is not a case class.")
    }

    val classType = instanceMirror.symbol.typeSignature
    val constructorSymbol = classType.decl(termNames.CONSTRUCTOR.asInstanceOf[scala.reflect.runtime.universe.Name]).asMethod
    val constructorMirror = mirror.reflectClass(classSymbol.asClass).reflectConstructor(constructorSymbol)

    // Collecting parameters from the constructor
    val params = constructorSymbol.paramLists.flatten

    val fieldValues = params.map { param =>
      val fieldTerm = classType.decl(param.name).asTerm.accessed.asTerm
      val fieldValue = instanceMirror.reflectField(fieldTerm).get
      func(fieldValue)
    }

    constructorMirror(fieldValues: _*).asInstanceOf[Expr]
  }

  def replaceWithMappingFunc(template_mappings: List[(Type, Type)]): Type => Type = {
    return {
      case t@Type.T(i) => {
        template_mappings.find(x => t == x._1) match {
          case Some((_, typeToReplaceWith)) => typeToReplaceWith
          case None => throw new Exception(s"T$i template type could not be replaced")
        }
      }
      case x => x
    }
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
      case ((codeLeft, Type.Num(), vLeft), (codeRight, Type.NumFloat(), vRight)) => {
        val con = s"${varc.next()} = sitofp ${Type.toLLVM(Type.Num())} $vLeft to ${Type.toLLVM(Type.NumFloat())}\n"
        if(command == "sdiv") floatBinOpTemplate(codeLeft+con, varc.last(), codeRight, vRight, "div")
        else floatBinOpTemplate(codeLeft+con, varc.last(), codeRight, vRight, command)
      }
      case ((codeLeft, Type.NumFloat(), vLeft), (codeRight, Type.Num(), vRight)) => {
        val con = s"${varc.next()} = sitofp ${Type.toLLVM(Type.Num())} $vRight to ${Type.toLLVM(Type.NumFloat())}\n"
        if(command == "sdiv") floatBinOpTemplate(codeLeft, vLeft, codeRight+con, varc.last(), "div")
        else floatBinOpTemplate(codeLeft, vLeft, codeRight+con, varc.last(), command)
      }
      case ((codeLeft, tyLeft: Type.Interface, vLeft), (codeRight, tyRight: Type.Interface, vRight)) if (tyLeft == tyRight && command == "add") =>
        convert(Expr.CallObjFunc(Expr.Compiled(codeLeft, tyLeft, vLeft), Expr.CallF("__add__", List(Expr.Compiled(codeRight, tyRight, vRight)), List())), env)
      case ((codeLeft, typeLeft, x), (codeRight, typeRight, y)) => throw new Exception(s"can't perform arithmetic on operands of types ${typeLeft} and ${typeRight}");
    }
  }

  def printTemplate(format: String, ty: String, loc: String): String = {
    s"%ret.${varc.extra()} = call i32 (ptr, ...) @printf(ptr @$format, $ty ${loc})\n" +
      s"${varc.next()} = load ptr, ptr @stdout, align 8\n" +
    s"call void @setbuf(ptr noundef ${varc.last()}, ptr noundef null)\n"
  }
  def printInterp(toPrint: Expr, env: Env): String = {
    val converted = convertLoc(toPrint, env)
    converted._2 match {
      case Type.Num() => converted._1 + printTemplate("format_num", "i32", converted._3);
      case Type.Bool() => converted._1 + printTemplate("format_num", "i1", converted._3);
      case Type.NumFloat() => converted._1 + printTemplate("format_float", "double", converted._3);
      case Type.Str() => converted._1 + printTemplate("format_string", "ptr", converted._3);
      case Type.Character() => converted._1 + printTemplate("format_char", "i8", converted._3);
      case Type.Interface(_,_,_,_) => convert(Expr.CallObjFunc(Expr.Compiled(converted._1, converted._2, converted._3), Expr.CallF("__print__", List(), List())), env)._1
      /*
      case Type.Bool() => converted._1 + s"cmp rax, 0\nje bool_${ifCounter}\n" + printTemplate("format_true") +
        s"jmp boole_${ifCounter}\nbool_${ifCounter}:\n" + printTemplate("format_false") + s"boole_${ifCounter}:\n";

      case Type.Array(Type.Character()) => converted._1 + "add rax, 8\n" + printTemplate("format_string");
       */
      //case Type.Array(_) => converted._1 + s"${varc.next()} = ptrtoint ptr ${converted._3} to i64\n" + printTemplate("format_uint", "i64", varc.last());
      case _ => throw new Exception(s"input of type ${converted._2} not recognized in print")
    }
  }

}

object ToAssembly extends ToAssembly("") {
  case class FunctionInfo(name: String, prefix: String, args: List[InputVar], retType: Type, templates: List[Type])

  case class InterfaceInfo(name: String, args: List[InputVar], funcs: List[FunctionInfo], templates: List[Type])

  case class EnumInfo(name: String, el: List[String])

  case class Variable(loc: String, varType: Type)

  case class FileDeclaration(functions: List[FunctionInfo], interfaces: List[InterfaceInfo])
}

class Counter {
  var counter = 1;
  private var pausedCounter = 1;
  private var counterExtra = 0;
  private var paused = false;
  def next(): String = {
    val cur = counter;
    if(!paused) counter += 1;
    s"%l$cur";
  }
  def extra(): Int = {
    counterExtra += 1;
    counterExtra - 1;
  }
  def last(): String = s"%l${counter-1}";
  def secondLast(): String = s"%l${counter-2}";
  def reset(): Unit = {
    counter = 1;
  }
  def pause(): Unit = {
    paused = true;
    pausedCounter = counter;
  }
  def unpause(): Unit = {
    paused = false;
    counter = pausedCounter;
  }
}

