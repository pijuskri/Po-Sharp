package scala

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
    converted += "format_string:\n        db  \"%s\", 10, 0\n"
    converted += "format_char:\n        db  \"%c\", 10, 0\n"
    converted += stringLiterals.mkString
    converted = converted.split("\n").zipWithIndex.foldLeft("")((acc, v)=> acc +s"\nline${v._2}:\n"+ v._1)
    converted
  }
  var lineNr = 0;
  var ifCounter = 0;
  var subconditionCounter: Int = 0;
  var stringLiterals: List[String] = List()
  var functions: List[FunctionInfo] = List();
  var functionScope: FunctionInfo = FunctionInfo("main", List(), Type.Num());
  private def convert(input: Expr, reg: List[String], env: Env): (String, Type) = {
    val conv = (_input: Expr) => convert(_input, reg, env)
    input match {
      case Expr.Num(value) => (s"mov ${reg.head}, 0${value}d\n", Type.Num())
      case Expr.NumFloat(value) => {
        (s"mov ${reg.head}, __float64__(${value.toString})\n", Type.NumFloat())
      }
      case Expr.Plus(left, right) => aritTemplate(left, right, "add", "addsd", reg, env)
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
        val look = lookup(name, env)
        (s"mov ${reg.head}, ${look._1}\n", look._2.varType)
      }
      case Expr.Block(lines) => convertBlock(lines, reg, env);
      case Expr.DefineArray(size, elemType, defaultValues) => conv(size) match {
        case (code, Type.Num()) => defineArray(code, elemType, defaultValues, env)
        case (code, x) => throw new Exception(s"not number when defining array size, got input of type $x")
      }
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
      //case Expr.Str(value) => (defineString(value, reg), Type.Str())
      case Expr.Str(value) => (defineArrayKnown(value.length, Type.Character(), value.map(x=>Expr.Character(x)).toList, env)._1, Type.Array(Type.Character()))
      case Expr.Character(value) => (s"mov ${reg.head}, ${value.toInt}\n", Type.Character())
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
    (look._2.varType, raxType) match {
      case (Type.Undefined(), Type.Undefined()) => {}
      case (Type.Undefined(), ass) => newenv = env.map(x=>if(x._1==name) (x._1, Variable(x._2.pointer, raxType)) else x)
      case (x,y) => throw new Exception(s"trying to set variable of type ${look._2.varType} to $raxType")
    }
    /*
    if(look._2.varType == Type.Undefined()) raxType match {
      case Type.Undefined() => newenv =
      case Type.Array(Type.Undefined()) => newenv = env.map(x=>if(x._1==name) (x._1, Variable(x._2.pointer, Type.Array(raxType))) else x)
      case x => println(x);
    }
    else if(look._2.varType != raxType) throw new Exception(s"trying to set variable of type ${look._2.varType} to $raxType")
     */

    (s"mov qword ${look._1}, rax\n", newenv)
  }
  //TODO add runtime index checking
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
  def getArray(name: Expr.Ident, index: Expr, reg: List[String], env: Env): (String, Type) = (convert(name, reg, env), convert(index, reg, env)) match {
    case ((code, Type.Array( arrType)), (indexCode, indexType)) => {
      if(indexType != Type.Num()) throw new Exception(s"wrong index for array, got $indexType")
      val size = arraySizeFromType(arrType);
      (code + s"push ${reg.head}\n" + indexCode + s"mov ${reg.tail.head}, ${reg.head}\n" +
        s"pop ${reg.head}\n" + s"mov ${sizeToReg(size, reg.head)}, [${reg.head}+8+${reg.tail.head}*$size]\n", arrType)
    }
    case ((code, varType), l) => throw new Exception(s"trying to access variable ${name.name} as an array, has type $varType")
  }
  def getArraySize(name: Expr.Ident, reg: List[String], env: Env): (String, Type) = convert(name, reg, env) match {
    case (code, Type.Array(arrType)) => {
      (code + getArrayDirect(reg.head, 0, 8, reg), Type.Num())
    }
    case (code, varType) => throw new Exception(s"trying to access variable ${name.name} as an array, has type $varType")
  }
  //TODO not safe when default values use rdi
  def setArrayDirect(code: String, index: Int, size: Int): String = {
    s"mov rdi, ${code}\n" + s"mov [rdi+${index*size}], ${sizeToReg(size, "rax")}\n"
  }
  def getArrayDirect(code: String, index: Int, size: Int, reg: List[String]): String = {
    s"mov ${reg.tail.head}, ${code}\n" + s"mov ${reg.head}, [${reg.tail.head}+${index*size}]\n"
  }
  val fullToByteReg: Map[String, String] = Map(("rax", "al"), ("rdi", "dil"))
  def sizeToReg(size: Int, reg: String): String = size match {
    case 8 => reg
    case 1 => fullToByteReg.getOrElse(reg, reg)
  }
  def arraySizeFromType(valtype: Type): Int = valtype match {
    case Type.Undefined() => 8
    case Type.Character() => 1
    case Type.Num() => 8
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
    converted._2 match {
      case Type.Num() => converted._1 + printTemplate("format_num");
      case Type.NumFloat() => converted._1 + "movq xmm0, rax\n" + "mov rdi, format_float\n" + "mov rax, 1\n" + "call printf\n"
      //case (Type.Str) => converted._1 + printTemplate("format_string");
      case Type.Character() => converted._1 + printTemplate("format_char");
      case Type.Array(Type.Character()) => converted._1 + "add rax, 8\n" + printTemplate("format_string");
      case _ => throw new Exception(s"input of type ${converted._2} not recognized in print")
    }
  }
  def freeMemory(env: Env): String = env.foldLeft("")((acc, entry) => entry._2.varType match {
    case Type.Array(arrType) => acc + s"mov rdi, [rbp-${entry._2.pointer}]\n" + "call free\n";
    case _ => acc
  })

  type Env = Map[String, Variable]
  case class FunctionInfo(name: String, args: List[Type], retType: Type)
  case class Variable(pointer: Int, varType: Type)
}

