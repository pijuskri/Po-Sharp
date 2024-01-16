package posharp

import posharp.ToAssembly.FunctionInfo

import scala.annotation.targetName

sealed trait Expr
object Expr{
  case class Str(s: String) extends Expr
  case class Ident(name: String) extends Expr
  case class Num(value: Int) extends Expr
  case class NumFloat(value: Float) extends Expr
  case class Character(char: Char) extends Expr

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

  case class SetVal(variable: Expr, value: Expr) extends Expr
  case class DefVal(variable: String, varType: Type) extends Expr
  case class DefValWithValue(variable: String, varType: Type, value: Expr) extends Expr
  case class Block(lines: List[Expr]) extends Expr
  case class ExtendBlock(lines: List[Expr]) extends Expr
  case class While(condition: Expr, execute: Expr.Block) extends Expr

  case class GetArray(array: Expr, index: Expr) extends Expr
  case class SetArray(array: Expr, index: Expr, value: Expr) extends Expr
  case class DefineArray(size: Expr, elemType:Type, defaultValues: List[Expr]) extends Expr
  case class ConcatArray(left: Expr, right: Expr) extends Expr
  case class ArraySize(array: Expr) extends Expr

  //case class StackVar(offset: Int) extends Expr
  case class Func(name: String, argNames: List[InputVar], retType: Type, body: Expr.Block, templates: List[Type.T]) extends Expr
  case class Lambda(argNames: List[InputVar], retType: Type, body: Expr.Block) extends Expr
  case class CallF(name: String, args: List[Expr], templates: List[Type]) extends Expr
  case class Return(value: Option[Expr]) extends Expr

  //currently code needs templates to be generic type. idk how to do it while enforcing template type
  case class DefineInterface(name: String, props: List[InputVar], functions: List[Func], templates: List[Type]) extends Expr
  case class InstantiateInterface(intf: String, args: List[Expr], templates: List[Type]) extends Expr
  case class GetProperty(obj: Expr, prop: String) extends Expr
  case class CallObjFunc(obj: Expr, func: CallF) extends Expr
  case class SetInterfaceProp(intf: Expr, prop: String, value: Expr) extends Expr

  /*
  case class DefineInterface(name: String, props: List[InputVar]) extends Expr
  case class InstantiateInterface(intf: String, defaultValues: List[Expr]) extends Expr
  case class GetProperty(obj: Expr, prop: String) extends Expr
  case class SetInterfaceProp(intf: Expr, prop: String, value: Expr) extends Expr
   */

  case class DefineEnum(name: String, props: List[String])

  case class Convert(value: Expr, to: Type) extends Expr

  case class TopLevel(functions: List[Func], interfaces: List[DefineInterface], enums: List[DefineEnum], imports: List[Import]) extends Expr
  case class Import(toImport: String, file: String) extends Expr

  case class Print(value: Expr) extends Expr
  case class Free(value: Expr) extends Expr

  case class ThrowException(errorMsg: String) extends Expr
  case class Nothing() extends Expr
  case class Compiled(code: String, raxType: Type, loc: String) extends Expr
  case class RawReference() extends Expr
}

sealed trait Type
object Type {
  case class Undefined() extends Type
  case class Num() extends Type
  case class NumFloat() extends Type
  case class Character() extends Type
  case class Array(elemType: Type) extends Type
  case class Bool() extends Type
  case class Str() extends Type
  case class Interface(name: String, properties: List[InputVar], functions: List[FunctionInfo], templates: List[Type]) extends Type
  case class StaticInterface(properties: List[InputVar], functions: List[FunctionInfo]) extends Type
  case class Function(args: List[Type], retType: Type) extends Type
  case class T(num: Int) extends Type
  case class Enum(el: List[String]) extends Type
  //https://stackoverflow.com/questions/10373715/scala-ignore-case-class-field-for-equals-hascode
  case class Closure(func: Function)(val env: List[(String, Type)]) extends Type
  object Closure {
    @targetName("custom_closure_apply")
    def apply(func: Function, env: List[(String, Type)]): Closure = Closure(func)(env)
    /*
    def unapply(u: Closure): Option[(Function, List[(String, Type)])] =
      if (u eq null) None
      else Some((u.func, u.env))
     */
    def unapply(u: Closure): Option[(Function, List[(String, Type)])] =
      Some(( u.func, u.env))

  }

  case class UserType(name: String, templates: List[Type]) extends Type

  def shortS(value: Type): String = value match {
    case Num() => "i";
    case NumFloat() => "f";
    case Character() => "c"
    case Bool() => "b"
    case Array(inner) => "arr_"+shortS(inner)+"_"
    //case Interface(inner) => "itf_"+inner.map(x=>shortS(x.varType))+"_"
    case Interface(_, inner, innerf, templates) => "itf_"+inner.map(x=>shortS(x.varType)).mkString+"_"+innerf.map(x=>x.name).mkString+"_"+ templates.map(x=>shortS(x)).mkString
    case Function(args, retType) => "func_"+args.map(x=>shortS(x)).mkString+"_"+shortS(retType)
    case UserType(name, templates) => name + templates.map(x=>shortS(x)).mkString
    //case T(a) => s"T$a"
    case Closure(func, env) => shortS(func)+env.map(x=>shortS(x._2)).mkString
    case Undefined() => "v"
    case _ => throw new Exception(s"$value unrecognised");
  }
  def compare(val1: Type, val2: Type): Boolean = (val1, val2) match {
    case (a,b) if a == b => true
    case _ => false
  }
  def compare(value: (Type, Type)): Boolean = compare(value._1,value._2)
  def compare(val1: List[Type], val2: List[Type]): Boolean = val1.length == val2.length && (val1 zip val2).forall(x=>compare(x))
  def defaultValue(valType: Type): Expr = valType match {
    case Num() => Expr.Num(0);
    case NumFloat() => Expr.NumFloat(0);
    case Character() => Expr.Character('\u0000');
    case _ => Expr.Nothing();
  }
  def toLLVM(valType: Type): String = valType match {
    case Num() => "i32";
    case NumFloat() => "double";
    case Character() => "i8"
    case Bool() => "i1"
    case Array(inner) => s"{i32, ${toLLVM(inner)}*}*"//s"%Type.array.${toLLVM(inner)}*"
    case Undefined() => "void"
    case Str() => "i8*"
    case Function(args, retType) => s"${toLLVM(retType)} (${args.map(x=>toLLVM(x)).mkString(",")})*"
    //presume that usertype is a class. Might have aliases in the future
    case UserType(name, templates) => s"%Class.$name.${templates.map(x=>toLLVM(x)).mkString}*"
    case Interface(name, _, _, templates) => s"%Class.$name.${templates.map(x=>toLLVM(x)).mkString}*"
    case Closure(func, env) => s"{${toLLVM(func)}, {${env.map(x=>toLLVM(x._2)).mkString(",")}}}*"
    /*
    case Interface(vars, funcs) => {
      val argS = vars.map(x=>toLLVM(x.varType)).mkString(", ")
      s"{ $argS }"
    }
     */
    case _ => throw new Exception(s"$valType unrecognised for LLVM");
  }
}
case class InputVar(name: String, varType: Type)
case class ObjVal(name: String, varType: Type, defaultValue: Expr)
//case class ObjFunc(name: String, args: List[InputVar], ret: Type)