package cs320

object Implementation extends Template {

  def typeCheck(e: Typed.Expr): Typed.Type = T.typeCheck(e)
  def interp(e: Untyped.Expr): Untyped.Value = U.interp(e)

  object T {
    import Typed.{TypeDef => TDef, _}

    case class Scheme(tvars: List[String], typ: Type)
    type VarEnv = Map[String, (Scheme, Boolean)]
    type TypeEnv = Map[String, TDef]

    case class Env(vars: VarEnv, types: TypeEnv, tvars: Set[String])
    private val emptyEnv = Env(Map.empty, Map.empty, Set.empty)

    private def wfType(t: Type, env: Env): Unit = t match {
      case IntT | BooleanT | UnitT => ()
      case VarT(a) =>
        if (!env.tvars.contains(a)) error()
      case AppT(n, ts) =>
        val td = env.types.getOrElse(n, error())
        if (td.tparams.length != ts.length) error()
        ts.foreach(wfType(_, env))
      case ArrowT(ps, rt) =>
        ps.foreach(wfType(_, env))
        wfType(rt, env)
    }

    private def subst(t: Type, m: Map[String, Type]): Type = t match {
      case IntT | BooleanT | UnitT => t
      case VarT(a) => m.getOrElse(a, t)
      case AppT(n, ts) => AppT(n, ts.map(subst(_, m)))
      case ArrowT(ps, r) => ArrowT(ps.map(subst(_, m)), subst(r, m))
    }

    private def tbind(d: RecDef, env: Env): Env = d match {
      case Lazy(x, t, _) =>
        env.copy(vars = env.vars + (x -> (Scheme(Nil, t), false)))

      case RecFun(x, tps, ps, rt, _) =>
        val ft = ArrowT(ps.map(_._2), rt)
        env.copy(vars = env.vars + (x -> (Scheme(tps, ft), false)))

      case TDef(tn, tps, vs) =>
        if (env.types.contains(tn)) error()
        val typeEnv = env.types + (tn -> TDef(tn, tps, vs))
        val ctorEnv = vs.foldLeft(env.vars) {
          case (m, Variant(vn, Nil)) =>
            m + (vn -> (Scheme(tps, AppT(tn, tps.map(VarT))), false))
          case (m, Variant(vn, ps)) =>
            val rt = AppT(tn, tps.map(VarT))
            m + (vn -> (Scheme(tps, ArrowT(ps, rt)), false))
        }
        env.copy(vars = ctorEnv, types = typeEnv)
    }

    private def checkRecDef(d: RecDef, env: Env): Unit = d match {
      case Lazy(_, t, e) =>
        wfType(t, env)
        if (typeCheck(e, env) != t) error()

      case RecFun(_, tps, ps, rt, b) =>
        if (tps.exists(env.tvars.contains)) error()
        val env1 = env.copy(tvars = env.tvars ++ tps)
        ps.foreach { case (_, t) => wfType(t, env1) }
        wfType(rt, env1)

        val env2 = ps.foldLeft(env1) {
          case (e, (p, t)) =>
            e.copy(vars = e.vars + (p -> (Scheme(Nil, t), false)))
        }

        if (typeCheck(b, env2) != rt) error()

      case TDef(_, tps, vs) =>
        if (tps.exists(env.tvars.contains)) error()
        val env1 = env.copy(tvars = env.tvars ++ tps)
        vs.foreach(v => v.params.foreach(wfType(_, env1)))
    }

    private def typeCheckCase(
      c: Case,
      vs: List[Variant],
      tps: List[String],
      tas: List[Type],
      env: Env
    ): Type = {
      val Variant(_, ps) = vs.find(_.name == c.variant).getOrElse(error())
      if (ps.length != c.names.length) error()
      val sub = tps.zip(tas).toMap
      val env1 = c.names.zip(ps).foldLeft(env) {
        case (e, (x, t)) =>
          e.copy(vars = e.vars + (x -> (Scheme(Nil, subst(t, sub)), false)))
      }
      typeCheck(c.body, env1)
    }

    def typeCheck(expr: Expr): Type =
      typeCheck(expr, emptyEnv)

    private def typeCheck(expr: Expr, env: Env): Type = expr match {
      case Id(x, tas) =>
        val (Scheme(tps, t), _) = env.vars.getOrElse(x, error())
        if (tps.length != tas.length) error()
        tas.foreach(wfType(_, env))
        subst(t, tps.zip(tas).toMap)

      case IntE(_) => IntT
      case BooleanE(_) => BooleanT
      case UnitE => UnitT

      case Add(l, r) =>
        if (typeCheck(l, env) != IntT || typeCheck(r, env) != IntT) error()
        IntT
      case Mul(l, r) =>
        if (typeCheck(l, env) != IntT || typeCheck(r, env) != IntT) error()
        IntT
      case Div(l, r) =>
        if (typeCheck(l, env) != IntT || typeCheck(r, env) != IntT) error()
        IntT
      case Mod(l, r) =>
        if (typeCheck(l, env) != IntT || typeCheck(r, env) != IntT) error()
        IntT
      case Eq(l, r) =>
        if (typeCheck(l, env) != IntT || typeCheck(r, env) != IntT) error()
        BooleanT
      case Lt(l, r) =>
        if (typeCheck(l, env) != IntT || typeCheck(r, env) != IntT) error()
        BooleanT

      case Sequence(l, r) =>
        typeCheck(l, env)
        typeCheck(r, env)

      case If(c, t, f) =>
        if (typeCheck(c, env) != BooleanT) error()
        val tt = typeCheck(t, env)
        if (typeCheck(f, env) != tt) error()
        tt

      case Val(mut, x, topt, e, b) =>
        val t1 = typeCheck(e, env)
        topt.foreach { t =>
          wfType(t, env)
          if (t != t1) error()
        }
        typeCheck(b, env.copy(vars = env.vars + (x -> (Scheme(Nil, t1), mut))))

      case Assign(x, e) =>
        env.vars.get(x) match {
          case Some((Scheme(Nil, t), true)) =>
            if (typeCheck(e, env) != t) error()
            UnitT
          case _ => error()
        }

      case Fun(ps, b) =>
        ps.foreach { case (_, t) => wfType(t, env) }
        val env1 = ps.foldLeft(env) {
          case (e, (x, t)) =>
            e.copy(vars = e.vars + (x -> (Scheme(Nil, t), false)))
        }
        ArrowT(ps.map(_._2), typeCheck(b, env1))

      case App(f, as) =>
        typeCheck(f, env) match {
          case ArrowT(ps, rt) =>
            if (ps.length != as.length) error()
            ps.zip(as).foreach {
              case (pt, a) =>
                if (typeCheck(a, env) != pt) error()
            }
            rt
          case _ => error()
        }

      case RecBinds(ds, b) =>
        val env1 = ds.foldLeft(env) { case (e, d) => tbind(d, e) }
        ds.foreach(d => checkRecDef(d, env1))
        val t = typeCheck(b, env1)
        wfType(t, env)
        t

      case Match(e, cs) =>
        typeCheck(e, env) match {
          case AppT(tn, tas) =>
            val td = env.types.getOrElse(tn, error())
            if (td.variants.length != cs.length) error()
            val ts = cs.map(typeCheckCase(_, td.variants, td.tparams, tas, env))
            ts.reduce((a, b) => if (a != b) error() else a)
          case _ => error()
        }
    }
  }

  object U {
    import Untyped._

    private case class State(store: Map[Addr, Value], next: Addr)

    private def alloc(st: State): (Addr, State) =
      (st.next, st.copy(next = st.next + 1))

    def interp(expr: Expr): Value =
      eval(expr, Map.empty, State(Map.empty, 0))._1

    private def force(a: Addr, st: State): (Value, State) =
      st.store.getOrElse(a, error()) match {
        case ExprV(e, env0) =>
          val (v, st1) = eval(e, env0, st)
          (v, st1.copy(store = st1.store.updated(a, v)))
        case v => (v, st)
      }

    private def eval(e: Expr, env: Env, st: State): (Value, State) = e match {

      case Id(x) => force(env.getOrElse(x, error()), st)
      case IntE(n) => (IntV(n), st)
      case BooleanE(b) => (BooleanV(b), st)
      case UnitE => (UnitV, st)

      case Add(l, r) =>
        val (IntV(a), st1) = eval(l, env, st)
        val (IntV(b), st2) = eval(r, env, st1)
        (IntV(a + b), st2)

      case Mul(l, r) =>
        val (IntV(a), st1) = eval(l, env, st)
        val (IntV(b), st2) = eval(r, env, st1)
        (IntV(a * b), st2)

      case Div(l, r) =>
        val (IntV(a), st1) = eval(l, env, st)
        val (IntV(b), st2) = eval(r, env, st1)
        if (b == 0) error()
        (IntV(a / b), st2)

      case Mod(l, r) =>
        val (IntV(a), st1) = eval(l, env, st)
        val (IntV(b), st2) = eval(r, env, st1)
        if (b == 0) error()
        (IntV(a % b), st2)

      case Eq(l, r) =>
        val (IntV(a), st1) = eval(l, env, st)
        val (IntV(b), st2) = eval(r, env, st1)
        (BooleanV(a == b), st2)

      case Lt(l, r) =>
        val (IntV(a), st1) = eval(l, env, st)
        val (IntV(b), st2) = eval(r, env, st1)
        (BooleanV(a < b), st2)

      case Sequence(l, r) =>
        val (_, st1) = eval(l, env, st)
        eval(r, env, st1)

      case If(c, t, f) =>
        val (BooleanV(b), st1) = eval(c, env, st)
        if (b) eval(t, env, st1) else eval(f, env, st1)

      case Val(x, e, b) =>
        val (v, st1) = eval(e, env, st)
        val (a, st2) = alloc(st1)
        eval(b, env + (x -> a), st2.copy(store = st2.store.updated(a, v)))

      case Assign(x, e) =>
        val (v, st1) = eval(e, env, st)
        val a = env.getOrElse(x, error())
        (UnitV, st1.copy(store = st1.store.updated(a, v)))

      case Fun(ps, b) =>
        (CloV(ps, b, env), st)

      case App(f, as) =>
        val (fv, st1) = eval(f, env, st)
        val (vs, st2) = as.foldLeft((List.empty[Value], st1)) {
          case ((acc, s), a) =>
            val (v, s1) = eval(a, env, s)
            (acc :+ v, s1)
        }
        fv match {
          case CloV(ps, body, fenv) =>
            val (addrs, st3) = ps.foldLeft((List.empty[Addr], st2)) {
              case ((acc, s), _) =>
                val (a, s1) = alloc(s)
                (acc :+ a, s1)
            }
            val env1 = fenv ++ ps.zip(addrs)
            val store1 = st3.store ++ addrs.zip(vs)
            eval(body, env1, st3.copy(store = store1))

          case ConstructorV(n) =>
            (VariantV(n, vs), st2)

          case _ => error()
        }

      case Match(e, cs) =>
        val (VariantV(n, vs), st1) = eval(e, env, st)
        val c = cs.find(_.variant == n).getOrElse(error())
        val (addrs, st2) = c.names.foldLeft((List.empty[Addr], st1)) {
          case ((acc, s), _) =>
            val (a, s1) = alloc(s)
            (acc :+ a, s1)
        }
        val env1 = env ++ c.names.zip(addrs)
        val store1 = st2.store ++ addrs.zip(vs)
        eval(c.body, env1, st2.copy(store = store1))

      case RecBinds(ds, b) =>
        val names = ds.flatMap {
          case Lazy(x, _) => List(x)
          case RecFun(x, _, _) => List(x)
          case TypeDef(vs) => vs.map(_.name)
        }
        val (addrs, st1) = names.foldLeft((List.empty[Addr], st)) {
          case ((acc, s), _) =>
            val (a, s1) = alloc(s)
            (acc :+ a, s1)
        }
        val env1 = env ++ names.zip(addrs)
        val store1 = ds.foldLeft(st1.store) {
          case (m, Lazy(x, e)) =>
            m.updated(env1(x), ExprV(e, env1))
          case (m, RecFun(x, ps, b)) =>
            m.updated(env1(x), CloV(ps, b, env1))
          case (m, TypeDef(vs)) =>
            vs.foldLeft(m) {
              case (mm, Variant(n, true)) =>
                mm.updated(env1(n), VariantV(n, Nil))
              case (mm, Variant(n, false)) =>
                mm.updated(env1(n), ConstructorV(n))
            }
        }
        eval(b, env1, st1.copy(store = store1))
    }
  }
}
