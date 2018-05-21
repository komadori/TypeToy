package typetoy

trait ToyError

case class Symbol(name : String) extends Ordered[Symbol] {
  def compare(that:Symbol) = name compare that.name
}

sealed trait Type

case class TVar(id : Integer) extends Type with Ordered[TVar] {
  def compare(that:TVar) = id - that.id
}

case class TCon(name : String) extends Type
case class TApp(lhs : Type, rhs : Type) extends Type

object TypeFunctions {
  def printApp (t : TApp) : String =
    t match {
      case TApp(l@TApp(_,_), r) => s"${printApp(l)} ${print(r)}"
      case TApp(l, r) => s"${print(l)} ${print(r)}"
    }

  def print (t : Type) : String =
    t match {
      case TVar(id) => s"t$id"
      case TCon(name) => name
      case TApp(l@TApp(_,_), r) => s"(${printApp(l)} ${print(r)})"
      case TApp(l, r) => s"(${print(l)} ${print(r)})"
    }
}
