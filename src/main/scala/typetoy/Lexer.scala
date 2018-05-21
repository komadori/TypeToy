package typetoy

import scala.util.parsing.combinator._

case class LexerError(msg : String) extends ToyError

sealed trait Token

case object LetTkn extends Token
case object RecTkn extends Token
case object EqTkn extends Token
case object InTkn extends Token
case object FunTkn extends Token
case object ArrTkn extends Token
case object BraTkn extends Token
case object KetTkn extends Token
case class SymTkn(sym : Symbol) extends Token

object Lexer extends RegexParsers
{
  override def skipWhitespace: Boolean = true
  def let = "let" ^^ { _ => LetTkn }
  def rec = "rec" ^^ { _ => RecTkn }
  def eq = "=" ^^ { _ => EqTkn }
  def in = "in" ^^ { _ => InTkn }
  def fun = "fun" ^^ { _ => FunTkn }
  def arr = "->" ^^ { _ => ArrTkn }
  def bra = "(" ^^ { _ => BraTkn }
  def ket = ")" ^^ { _ => KetTkn }
  def sym = "[A-Za-z][A-Za-z0-9]*".r ^^ { n => SymTkn(Symbol(n)) }
  def tokens= phrase(rep(let | rec | eq | in | fun | arr | bra | ket | sym))
  def apply(input : String) = {
    parse(tokens, input) match {
      case Success(tkns, _) => Right(tkns)
      case NoSuccess(msg, _) => Left(LexerError(msg))
    }
  }
}


