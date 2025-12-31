package cs320

import Value._
import Expr._

object Implementation extends Template {

  sealed trait Handler
  case object NoHandler extends Handler
  case class SomeHandler(
    handlerExpr: Expr,
    savedEnv: Env,
    savedK: Cont,
    prev: Handler
  ) extends Handler

  def interp(expr: Expr): Value =
    eval(expr, Map.empty, v => v, NoHandler)

  def eval(e: Expr, env: Env, k: Cont, h: Handler): Value = e match {

    case Id(x) =>
      env.getOrElse(x, error(s"unbound variable: $x")) |> k

    case IntE(n) => k(IntV(n))

    case BooleanE(b) => k(BooleanV(b))

    case Add(l, r) =>
      eval(l, env, lv =>
        eval(r, env, rv =>
          (lv, rv) match {
            case (IntV(n1), IntV(n2)) => k(IntV(n1 + n2))
            case _ => error("Add Error")
          },
          h
        ),
        h
      )

    case Mul(l, r) =>
      eval(l, env, lv =>
        eval(r, env, rv =>
          (lv, rv) match {
            case (IntV(n1), IntV(n2)) => k(IntV(n1 * n2))
            case _ => error("Mul Error")
          },
          h
        ),
        h
      )

    case Div(l, r) =>
      eval(l, env, lv =>
        eval(r, env, rv =>
          (lv, rv) match {
            case (IntV(n1), IntV(n2)) =>
              if (n2 == 0) error("division by zero")
              else k(IntV(n1 / n2))
            case _ => error("Div Error")
          },
          h
        ),
        h
      )

    case Mod(l, r) =>
      eval(l, env, lv =>
        eval(r, env, rv =>
          (lv, rv) match {
            case (IntV(n1), IntV(n2)) =>
              if (n2 == 0) error("modulo by zero")
              else k(IntV(n1 % n2))
            case _ => error("Mod Error")
          },
          h
        ),
        h
      )

    case Eq(l, r) =>
      eval(l, env, lv =>
        eval(r, env, rv =>
          (lv, rv) match {
            case (IntV(a), IntV(b)) => k(BooleanV(a == b))
            case _ => error("Eq Error")
          },
          h
        ),
        h
      )

    case Lt(l, r) =>
      eval(l, env, lv =>
        eval(r, env, rv =>
          (lv, rv) match {
            case (IntV(a), IntV(b)) => k(BooleanV(a < b))
            case _ => error("Lt Error")
          },
          h
        ),
        h
      )

    case If(c, t, f) =>
      eval(c, env, cv => cv match {
        case BooleanV(true)  => eval(t, env, k, h)
        case BooleanV(false) => eval(f, env, k, h)
        case _ => error("If Error")
      }, h)

    case TupleE(es) =>
      evalList(es, env, vs => k(TupleV(vs)), h)

    case Proj(expr, idx) =>
      eval(expr, env, v => v match {
        case TupleV(vs) =>
          if (idx <= 0 || idx > vs.length)
            error("tuple projection out of range")
          else
            k(vs(idx - 1))
        case _ => error("projection Non-tuple")
      }, h)

    case NilE => k(NilV)

    case ConsE(hd, tl) =>
      eval(hd, env, hv =>
        eval(tl, env, tv => tv match {
          case NilV | ConsV(_, _) => k(ConsV(hv, tv))
          case _ => error("tail of cons Non-list")
        }, h),
      h)

    case Empty(expr) =>
      eval(expr, env, v => v match {
        case NilV        => k(BooleanV(true))
        case ConsV(_, _) => k(BooleanV(false))
        case _           => error("isEmpty Non-list")
      }, h)

    case Head(expr) =>
      eval(expr, env, v => v match {
        case ConsV(hd, _) => k(hd)
        case NilV         => error("head of empty list")
        case _            => error("head Non-list")
      }, h)

    case Tail(expr) =>
      eval(expr, env, v => v match {
        case ConsV(_, tl) => k(tl)
        case NilV         => error("tail of empty list")
        case _            => error("tail non-list")
      }, h)

    case Val(x, e1, e2) =>
      eval(e1, env, v1 =>
        eval(e2, env + (x -> v1), k, h),
      h)

    case Vcc(x, body) =>
      val captured = ContV(k)
      eval(body, env + (x -> captured), k, h)

    case Fun(ps, body) =>
      k(CloV(ps, body, env))

    case RecFuns(fds, body) =>
      val dummyEnv: Env = env
      val closures: List[(String, CloV)] =
        fds.map(fd => fd.name -> CloV(fd.parameters, fd.body, dummyEnv))

      val extendedEnv: Env =
        closures.foldLeft(env) { case (acc, (name, clo)) =>
          acc + (name -> clo)
        }

      closures.foreach { case (_, clo) =>
        clo.env = extendedEnv
      }

      eval(body, extendedEnv, k, h)

    case App(fun, args) =>
      eval(fun, env, fv =>
        evalList(args, env, argVals =>
          fv match {
            case CloV(ps, body, cloEnv) =>
              if (ps.length != argVals.length)
                error("arity mismatch")
              val newEnv = cloEnv ++ ps.zip(argVals)
              eval(body, newEnv, k, h)

            case ContV(k2) =>
              if (argVals.length != 1)
                error("continuation takes exactly 1 argument")
              k2(argVals.head)

            case _ =>
              error("calling non-function")
          },
          h
        ),
        h
      )

    case Test(expr, typ) =>
      eval(expr, env, v => {
        val ok = (typ, v) match {
          case (IntT,      IntV(_))                    => true
          case (BooleanT,  BooleanV(_))                => true
          case (TupleT,    TupleV(_))                  => true
          case (ListT,     NilV | ConsV(_, _))         => true
          case (FunctionT, CloV(_, _, _) | ContV(_))   => true
          case _ => false
        }
        k(BooleanV(ok))
      }, h)

    case Throw(expr) =>
      eval(expr, env, v =>
        h match {
          case NoHandler =>
            error("Runtime error")

          case SomeHandler(handlerExpr, savedEnv, savedK, prevH) =>
            eval(handlerExpr, savedEnv, hv => hv match {

              case CloV(List(x), body, cloEnv) =>
                val newEnv = cloEnv + (x -> v)
                eval(body, newEnv, savedK, prevH)

              case ContV(cont) =>
                cont(v)

              case _ =>
                error("handler is not a function or continuation")
            }, prevH)
        },
      h)

    case Try(e1, e2) =>
      val newH: Handler = SomeHandler(e2, env, k, h)
      eval(e1, env, k, newH)
  }

  def evalList(es: List[Expr], env: Env, k: List[Value] => Value, h: Handler): Value =
    es match {
      case Nil => k(Nil)
      case hd :: tl =>
        eval(hd, env, v =>
          evalList(tl, env, vs => k(v :: vs), h),
        h)
    }

  implicit class Pipe[A](x: A) {
    def |>[B](f: A => B): B = f(x)
  }
}
