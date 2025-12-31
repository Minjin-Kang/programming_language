package cs320

import Value._

object Implementation extends Template {

  def interp(expr: Expr): Value = {
    def eval(e: Expr, env: Env): Value = e match {
      case Id(x) => lookup(x, env)
      case IntE(n) => IntV(n)
      case BooleanE(e) => BooleanV(e) 
      
      case Add(l, r) => IntV(intVAdd(eval(l, env), eval(r, env)))
      case Mul(l, r) => IntV(intVMul(eval(l, env), eval(r, env)))
      case Div(l, r) => IntV(intVDiv(eval(l, env), eval(r, env)))
      case Mod(l, r) => IntV(intVMod(eval(l, env), eval(r, env)))

      case Eq(l, r) => BooleanV(eval(l, env) == eval(r, env))
      case Lt(l, r) => (eval(l, env), eval(r, env)) match {
        case(IntV(a), IntV(b)) => BooleanV(a < b)
        case _ => sys.error("Type error - Lt")
      }
      case If(c, t, f) => eval(c, env) match {
        case BooleanV(true) => eval(t, env)
        case BooleanV(false) => eval(f, env)
        case _ => sys.error("It's not Boolean type - If")
      }

      case TupleE(li) => TupleV(li.map(e => eval(e, env)))
      case Proj(e, i) => eval(e, env) match {
        case TupleV(li) => 
          if (i >= 1 && i <= li.length) li(i-1)
          else sys.error("Out of Range")
        case _ => sys.error("It's not a List - Proj")
      }
      case NilE => NilV
      case ConsE(h, t) => ConsV(eval(h, env), eval(t, env))
      case Empty(e) => eval(e, env) match {
        case NilV => BooleanV(true)
        case ConsV(_, _) => BooleanV(false)
        case _ => sys.error("It's not a List - Empty")
      }
      case Head(e) => eval(e, env) match {
        case ConsV(h, _) => h
        case _ => sys.error("It's not a List - Head")
      }
      case Tail(e) => eval(e, env) match {
        case ConsV(_, t) => t
        case _ => sys.error("It's not a List - Tail")
      }
      case Val(x, e, b) => 
        val y = eval(e, env)
        eval(b, env + (x -> y))
      case Fun(x, b) => CloV(x, b, env)
      case RecFuns(fs, b) => 
        lazy val nEnv: Env = env ++ cloVals
        lazy val cloVals: List[(String, Value)] =
          fs.map(f => f.name -> CloV(f.parameters, f.body, nEnv))
        eval(b, nEnv)
      case App(f, a) => eval(f, env) match {
        case CloV(p, b, nEnv) =>
          if (p.length != a.length) sys.error("Error - Argument Number")
          val argVals = a.map(x => eval(x, env))
          val newEnv = nEnv ++ p.zip(argVals)
          eval(b,newEnv)
        case _ => sys.error("Error - No function value")
      }
      case Test(e, t) => 
        val v = eval(e, env)
        val res = (t, v) match {
          case (IntT, IntV(_)) => true
          case (BooleanT, BooleanV(_)) => true
          case (TupleT, TupleV(_)) => true
          case (ListT, NilV | ConsV(_, _)) => true
          case (FunctionT, CloV(_, _, _)) => true
          case _ => false
        }
        BooleanV(res)
    }
    def lookup(name: String, env: Env): Value =
      env.getOrElse(name, sys.error(s"Unbound variable: $name"))
    def intVAdd(v1: Value, v2: Value): BigInt = (v1, v2) match {
      case (IntV(a), IntV(b)) => a + b
      case _ => sys.error("Type error - Add")
    }
    def intVMul(v1: Value, v2: Value): BigInt = (v1, v2) match {
      case (IntV(a), IntV(b)) => a * b
      case _ => sys.error("Type error - Mul")
    }
    def intVDiv(v1: Value, v2: Value): BigInt = (v1, v2) match {
      case (IntV(a), IntV(b)) =>
        if (b == 0) sys.error("Division by zero")
        else a / b
      case _ => sys.error("Type error - Div")
    }
    def intVMod(v1: Value, v2: Value): BigInt = (v1, v2) match {
      case (IntV(a), IntV(b)) =>
        if (b == 0) sys.error("Modulo by zero")
        else a % b
      case _ => sys.error("Type error - Mod")
    }
  eval(expr, Map())
  }
}