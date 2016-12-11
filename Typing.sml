structure Typing : TYPING = struct
  (* exception that arises when unbound variable occur *)
  exception UnboundVar of string

  (* perform type inference *)
  fun typingPat l env (Syntax.PCONS (c, ps)) t =
        (Type.unify (t, Cons.cod c);
         ListPair.foldlEq (fn (p, t, env) =>
           typingPat l env p t) env (ps, Cons.dom c))
    | typingPat l env Syntax.PWILD t = env
    | typingPat l env (Syntax.PVAR x) t =
        StringMap.insert (env, x, Type.toTypeScheme t)

  fun typingExp l env (Syntax.CONS (c, ms)) =
        (ListPair.appEq Type.unify (Cons.dom c, map (typingExp l env) ms);
         Cons.cod c)
    | typingExp l env (Syntax.VAR x) =
        (case StringMap.find (env, x) of
              NONE => raise (UnboundVar x)
            | SOME t => #1 (Type.inst l t))
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
        typingExp l (foldl (fn (d, env) =>
          StringMap.unionWith #2 (env, typingDec l env d)) env dec) m
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
    | typingExp l env (Syntax.PRIM (p, ms)) =
        (ListPair.appEq Type.unify (Prim.dom p, map (typingExp l env) ms);
         Prim.cod p)
  and typingDec l env (Syntax.VAL (x, m)) =
        StringMap.singleton (x, #1 (Type.generalize l (typingExp (l + 1) env m)))
    | typingDec l env (Syntax.VALREC xms) =
        let
          val xmts = map (fn (x, m) => (x, m, Type.genvar (l + 1))) xms
          val env' = foldl (fn ((x, _, t), env) =>
            StringMap.insert (env, x, Type.toTypeScheme t)) env xmts
        in
          app (fn (x, m, t) => Type.unify (t, typingExp (l + 1) env' m)) xmts;
          foldl (fn ((x, _, t), env) =>
            StringMap.insert (env, x, #1 (Type.generalize l t))) StringMap.empty xmts
        end

  val typingDec = typingDec 0
end
