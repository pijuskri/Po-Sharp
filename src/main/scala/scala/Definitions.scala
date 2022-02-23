package scala

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

  case class Print(value: Expr) extends Expr
  case class SetVal(variable: Expr, value: Expr) extends Expr
  case class DefVal(variable: Expr, varType: Type) extends Expr
  case class Block(lines: List[Expr]) extends Expr
  case class ExtendBlock(lines: List[Expr]) extends Expr
  case class While(condition: Expr, execute: Expr.Block) extends Expr

  case class GetArray(array: Ident, index: Expr) extends Expr
  case class SetArray(array: Ident, index: Expr, value: Expr) extends Expr
  case class DefineArray(size: Expr, elemType:Type, defaultValues: List[Expr]) extends Expr
  case class ConcatArray(left: Expr, right: Expr) extends Expr
  case class ArraySize(array: Ident) extends Expr

  //case class StackVar(offset: Int) extends Expr
  case class Func(name: String, argNames: List[InputVar], retType: Type, body: Expr.Block) extends Expr
  case class CallF(name: String, args: List[Expr]) extends Expr
  case class Return(value: Option[Expr]) extends Expr

  case class DefineInterface(name: String, props: List[InputVar]) extends Expr
  case class InstantiateInterface(intf: String, defaultValues: List[Expr]) extends Expr
  case class GetInterfaceProp(intf: Expr, prop: String) extends Expr
  case class SetInterfaceProp(intf: Expr, prop: String, value: Expr) extends Expr

  case class Convert(value: Expr, to: Type) extends Expr

  case class TopLevel(functions: List[Func], interfaces: List[DefineInterface]) extends Expr
  case class Nothing() extends Expr
}

sealed trait Type
object Type {
  case class Undefined() extends Type
  case class Num() extends Type
  case class NumFloat() extends Type
  case class Character() extends Type
  case class Array(elemType: Type) extends Type
  case class Interface(properties: List[InputVar]) extends Type

  //to be converted when parsing
  case class UserType(name: String) extends Type

  def shortS(value: Type): String = value match {
    case Num() => "i";
    case NumFloat() => "f";
    case Character() => "c"
    case Array(inner) => "arr_"+shortS(inner)+"_"
  }
  //case class Array(size: Int, elemType: Type) extends Type
}
case class InputVar(name: String, varType: Type)
