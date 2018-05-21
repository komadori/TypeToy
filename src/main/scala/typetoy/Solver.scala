package typetoy

import scala.collection.immutable._

case class SolverError(msg : String) extends ToyError

case class Subst(subst : TreeMap[TVar, Type])

object Solver {

  def occurs(x : TVar, t : Type) : Boolean =
    t match {
      case TVar(_) => t == x
      case TCon(_) => false
      case TApp(u, v) => occurs(x, u) || occurs(x, v)
    }

  def subst(s : Type, x : TVar, t : Type) : Type =
    t match {
      case TVar(_) => if (t == x) s else t
      case TCon(_) => t
      case TApp(u, v) => TApp(subst(s, x, u), subst(s, x, v))
    }

  def applySubst(s : Subst, t : Type) : Type =
    s.subst.foldRight(t)((xu, tt) => subst(xu._2, xu._1, tt))

  def nullSubst =
    Subst(TreeMap.empty[TVar, Type])

  def mkSubst(v : TVar, t : Type) =
    Subst(TreeMap.empty[TVar, Type].insert(v, t))

  def mergeSubst(u : Subst, v : Subst) =
    Subst(v.subst.foldRight(u.subst)((kv, s) => s.insert(kv._1, applySubst(u, kv._2))))

  def unifyVar(v : TVar, t : Type) : Either[SolverError, Subst] =
    if (occurs(v, t)) Left(SolverError("Infinite type")) else Right(mkSubst(v, t))

  def unifyOne(s : Type, t : Type) : Either[SolverError, Subst] =
    (s, t) match {
      case (v@TVar(_), _) => unifyVar(v, t)
      case (_, v@TVar(_)) => unifyVar(v, s)
      case (TApp(x, y), TApp(u, v)) => {
        (unifyOne(x, u), unifyOne(y, v)) match {
          case (Right(a), Right(b)) => Right(mergeSubst(a, b))
          case (Left(err), _) => Left(err)
          case (_, Left(err)) => Left(err)
        }
      }
      case (_, _) =>
        if (s == t) Right(nullSubst)
        else Left(SolverError(s"Not unifiable: '$s' and '$t'"))
    }

  def unifyList(cs : List[Constraint]) : Either[SolverError, Subst] =
    cs match {
      case Nil => Right(nullSubst)
      case (Constraint(x, y)::rest) => {
        unifyList(rest) match {
          case Right(t2) => {
            unifyOne(applySubst(t2, x), applySubst(t2, y)) match {
              case Right(t1) => Right(mergeSubst(t1, t2))
              case err => err
            }
          }
          case err => err
        }
      }
    }

  def apply(input : (List[Constraint], Type)) : Either[SolverError, Type] =
    input match {
      case (cs, v@TVar(_)) => {
        unifyList(cs) match {
          case Right(s) => {
            s.subst.get(v) match {
              case Some(ty) => Right(ty)
              case None => Left(SolverError("Missing root variable"))
            }
          }
          case Left(err) => Left(err)
        }
      }
      case (_, _) => Left(SolverError("Internal error"))
    }
}