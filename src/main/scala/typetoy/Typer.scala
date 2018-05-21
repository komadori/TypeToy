package typetoy

import scalaz.Scalaz._
import scalaz._

import scala.collection.immutable._

case class TyperError(msg : String) extends ToyError

case class Constraint(a : Type, b : Type)

object Typer
{
  type Inner[A] = ReaderWriterState[Unit, List[Constraint], TVar, A]
  type Res[T] = EitherT[Inner, TyperError, T]

  def newVar : Res[TVar] =
    EitherT[Inner, TyperError, TVar] {
      ReaderWriterState {
        (_, i) => {
         def j = i match { case TVar(n) => TVar(n+1) }
         (Nil, \/-(i), j)
        }
      }
    }

  def addConstraint(a : Type, b : Type) : Res[Unit] =
    EitherT[Inner, TyperError, Unit] {
      ReaderWriterState {
        (_, i) => {
          (List(Constraint(a, b)), \/-(()), i)
        }
      }
    }

  def value[A](value : A) : Res[A] =
    EitherT[Inner, TyperError, A] {
      ReaderWriterState {
        (_, s) => (Nil, \/-(value), s)
      }
    }

  def error[A](msg : String) : Res[A] =
    EitherT[Inner, TyperError, A] {
      ReaderWriterState {
        (_, s) => (Nil, -\/(TyperError(msg)), s)
      }
    }

  def mkArrow(a : Type, b : Type) : Type =
    TApp(TApp(TCon("->"), a), b)

  def process(node : Node, context : TreeMap[Symbol, Type]) : Res[Type] = {
    node match {
      case LetNode(sym, Simple, bound, body) =>
        {
          for {
            varTy <- process(bound, context)
            bodyTy <- process(body, context.insert(sym, varTy))
          } yield bodyTy
        }
      case LetNode(sym, Recursive, bound, body) =>
      {
        for {
          recTy <- newVar
          varTy <- process(bound, context.insert(sym, recTy))
          bodyTy <- process(body, context.insert(sym, recTy))
          _ <- addConstraint(recTy, varTy)
        } yield bodyTy
      }
      case FunNode(args, body) =>
        {
          for {
            r <- newVar
            argTys <- args.map((_:Symbol) => newVar).sequence
            bodyTy <- process(body, args.zip(argTys).foldLeft(context)((ctx, symTy) =>
              symTy match { case (sym, ty) => ctx.insert(sym, ty) }))
            _ <- addConstraint(r, argTys.foldRight(bodyTy)(mkArrow _))
          } yield r
        }
      case ApplyNode(func, args) =>
        {
          for {
            r <- newVar
            funcTy <- process(func, context)
            argTys <- args.map(arg => process(arg, context)).sequence
            _ <- addConstraint(funcTy, argTys.foldRight(r:Type)(mkArrow _))
          } yield r
        }
      case SymNode(sym) =>
        {
          context.get(sym) match {
            case Some(ty) => value(ty)
            case None => error("Unbound symbol")
          }
        }
    }
  }

  def apply (node : Node) =
  {
    process(node, TreeMap.empty[Symbol, Type]).run.run((), TVar(0)) match {
      case (cs, -\/(err), _) => Left(err)
      case (cs, \/-(ty), _) => Right(Tuple2(cs, ty))
    }
  }
}