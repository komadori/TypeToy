package typetoy

import scala.util.parsing.combinator._
import scala.util.parsing.input._

case class ParserError(msg : String) extends ToyError

sealed trait LetType

case object Simple extends LetType
case object Recursive extends LetType

sealed trait Node

case class LetNode(sym : Symbol, let : LetType, bound : Node, body : Node) extends Node
case class FunNode(args : List[Symbol], body : Node) extends Node
case class ApplyNode(func : Node, args : List[Node]) extends Node
case class SymNode(sym : Symbol) extends Node

class SeqReader[T](seq : Seq[T]) extends Reader[T] {
  override def first: T = seq.head
  override def atEnd: Boolean = seq.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[T] = new SeqReader(seq.tail)
}

object Parser extends Parsers {
  override type Elem = Token

  def sym = accept("SymTkn", { case s @ SymTkn(_) => s })

  def rec(x : Option[Token]) = x match
    {
      case Some(RecTkn) => Recursive
      case _ => Simple
    }

  def let = LetTkn ~ (RecTkn?) ~ sym ~ EqTkn ~ expr ~ InTkn ~ expr ^^ {
    case _ ~ r ~ SymTkn(name) ~ _ ~ bound ~ _ ~ body => LetNode(name, rec(r), bound, body)
  }

  def fun = FunTkn ~ rep1(sym) ~ ArrTkn ~ expr ^^ { case _ ~ args ~ _ ~ body => FunNode(args.map((x) => x.sym), body) }

  def nest = BraTkn ~ expr ~ KetTkn ^^ { case _ ~ x ~ _ => x }

  def leaf = sym ^^ { case SymTkn(s) => SymNode(s) }

  def subterm = nest | leaf

  def term : Parser[Node] = subterm ~ (subterm*) ^^ {
    case lhs ~ args => {
      args match {
        case Nil => lhs
        case _ => ApplyNode(lhs, args)
    }}}

  def expr : Parser[Node] = let | fun | term

  def program = phrase(expr)

  def apply (input : List[Token]) =
  {
    program(new SeqReader(input)) match {
      case Success(tree, _) => Right(tree)
      case NoSuccess(msg, _) => Left(ParserError(msg))
    }
  }
}