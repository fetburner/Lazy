structure Typing : TYPING = struct
  (* exception that arises when unbound variable occur *)
  exception UnboundVar of string

  (* perform type inference *)
  fun typingPat l env (Syntax.PINT _) t =
        (Type.unify (t, Type.INT); env)
    | typingPat l env (Syntax.PBOOL _) t =
        (Type.unify (t, Type.BOOL); env)
    | typingPat l env Syntax.PNIL t =
        (Type.unify (t, Type.LIST (Type.genvar l)); env)
    | typingPat l env (Syntax.PVAR x) t =
        StringMap.insert (env, x, Type.toTypeScheme t)
    | typingPat l env (Syntax.PCONS (p1, p2)) t =
        let val t' = Type.genvar l in
          Type.unify (t, Type.LIST t');
          typingPat l (typingPat l env p1 t') p2 t
        end
    | typingPat l env (Syntax.PTUPLE ps) t =
        let val pts = map (fn p => (p, Type.genvar l)) ps in
          Type.unify (t, Type.TUPLE (map #2 pts));
          foldl (fn ((p, t), env) => typingPat l env p t) env pts
        end

  fun typingExp l env (Syntax.INT _) = Type.INT
    | typingExp l env (Syntax.BOOL _) = Type.BOOL
    | typingExp l env Syntax.NIL = Type.LIST (Type.genvar l)
    | typingExp l env (Syntax.VAR x) =
        (case StringMap.find (env, x) of
              NONE => raise (UnboundVar x)
            | SOME t => Type.inst l t)
    | typingExp l env (Syntax.ABS (x, m)) =
        let
          val t1 = Type.genvar l
          val t2 = typingExp l (StringMap.insert (env, x, Type.toTypeScheme t1)) m
        in
          Type.ARROW (t1, t2)
        end
    | typingExp l env (Syntax.APP (m, n)) =
        let
          val t1 = typingExp l env m
          val t2 = typingExp l env n
          val t3 = Type.genvar l
        in
          Type.unify (t1, Type.ARROW (t2, t3));
          t3
        end
    | typingExp l env (Syntax.LET (dec, m)) =
        typingExp l (foldl (fn (d, env) => typingDec l env d) env dec) m
    | typingExp l env (Syntax.TUPLE ms) =
        Type.TUPLE (map (typingExp l env) ms)
    | typingExp l env (Syntax.CASE (m, pns)) =
        let
          val t1 = typingExp l env m
          val t2 = Type.genvar l
        in
          app (fn (p, n) =>
            Type.unify (t2,
              typingExp l
                (typingPat l env p t1) n)) pns;
          t2
        end
    | typingExp l env (Syntax.PLUS (m, n)) =
        let
          val t1 = typingExp l env m
          val t2 = typingExp l env n
        in
          Type.unify (t1, Type.INT);
          Type.unify (t2, Type.INT);
          Type.INT
        end
    | typingExp l env (Syntax.MINUS (m, n)) =
        let
          val t1 = typingExp l env m
          val t2 = typingExp l env n
        in
          Type.unify (t1, Type.INT);
          Type.unify (t2, Type.INT);
          Type.INT
        end
    | typingExp l env (Syntax.TIMES (m, n)) =
        let
          val t1 = typingExp l env m
          val t2 = typingExp l env n
        in
          Type.unify (t1, Type.INT);
          Type.unify (t2, Type.INT);
          Type.INT
        end
    | typingExp l env (Syntax.LE (m, n)) =
        let
          val t1 = typingExp l env m
          val t2 = typingExp l env n
        in
          Type.unify (t1, Type.INT);
          Type.unify (t2, Type.INT);
          Type.BOOL
        end
    | typingExp l env (Syntax.CONS (m, n)) =
        let
          val t1 = typingExp l env m
          val t2 = typingExp l env n
        in
          Type.unify (Type.LIST t1, t2);
          t2
        end
  and typingDec l env (Syntax.VAL (x, m)) =
        StringMap.insert (env, x, Type.generalize l (typingExp (l + 1) env m))
    | typingDec l env (Syntax.VALREC xms) =
        let
          val xts = map (fn (x, _) => (x, Type.genvar (l + 1))) xms
          val env' = foldl (fn ((x, t), env) =>
            StringMap.insert (env, x, Type.toTypeScheme t)) env xts
          val ts = map (fn (_, m) => typingExp (l + 1) env' m) xms
        in
          ListPair.app Type.unify (map #2 xts, ts);
          foldl (fn ((x, t), env) =>
            StringMap.insert (env, x, Type.generalize l t)) env xts
        end

  (* typing expression *)
  val typing = typingExp 0 StringMap.empty
end
