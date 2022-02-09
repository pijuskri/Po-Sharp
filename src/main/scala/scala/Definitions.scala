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
  case class Return(value: Expr) extends Expr

  case class Convert(value: Expr, to: Type) extends Expr

  case class TopLevel(functions: List[Func]) extends Expr
  case class Nothing() extends Expr
}

sealed trait Type
object Type {
  case class Undefined() extends Type
  case class Num() extends Type
  case class NumFloat() extends Type
  case class Character() extends Type
  //case class Str() extends Type
  //val Str = () => Array(Character())
  case class Array(elemType: Type) extends Type

  //case class Array(size: Int, elemType: Type) extends Type
}
case class InputVar(name: String, varType: Type)
