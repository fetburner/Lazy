structure Typing : TYPING = struct
  (* exception that arises when unbound variable occur *)
  exception UnboundVar of string

  (* perform type inference *)
  fun typingExp l env (Syntax.INT _) = Type.INT
    | typingExp l env (Syntax.BOOL _) = Type.BOOL
    | typingExp l env (Syntax.VAR x) =
        (case StringMap.find (env, x) of
              NONE => raise (UnboundVar x)
            | SOME t => Type.inst l t)
    | typingExp l env (Syntax.IF (m, n1, n2)) =
        let
          val t1 = typingExp l env m
          val t2 = typingExp l env n1
          val t3 = typingExp l env n2
        in
          Type.unify (t1, Type.BOOL);
          Type.unify (t2, t3);
          t2
        end
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
    | typingExp l env (Syntax.LET_TUPLE (m, xs, n)) =
        let
          val t1 = typingExp l env m
          val xs' = map (fn x => (x, Type.genvar l)) xs
          val t2 = typingExp l (foldl (fn ((x, t), s) =>
            StringMap.insert (s, x, Type.toTypeScheme t)) env xs') n
        in
          Type.unify (t1, Type.TUPLE (map #2 xs'));
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
  and typingDec l env (Syntax.VAL (x, m)) =
        StringMap.insert (env, x, Type.generalize l (typingExp (l + 1) env m))
    | typingDec l env (Syntax.VALREC (f, x, m)) =
        let
          val t1 = Type.genvar (l + 1)
          val t1' = typingExp (l + 1)
            (StringMap.insert (env, f, Type.toTypeScheme t1))
            (Syntax.ABS (x, m))
        in
          Type.unify (t1, t1');
          StringMap.insert (env, f, Type.generalize l t1)
        end

  (* typing expression *)
  val typing = typingExp 0 StringMap.empty
end
