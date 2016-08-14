structure Value : VALUE = struct
  exception PatternMatchFailure

  datatype value =
      INT of int
    | BOOL of bool
    | NIL
    | CONS of thunk * thunk
    (* closure *)
    | FUN of env * string * Syntax.exp
    | TUPLE of thunk list
  withtype thunk = (unit -> value) ref
  and env = thunk StringMap.map

  fun findMap f [] = NONE
    | findMap f (x :: xs) =
        (case f x of
              NONE => findMap f xs
            | r as SOME _ => r)

  fun thunkFromValue v = ref (fn () => v)

  (* obtain value from thunk *)
  fun force (thunk as ref f) =
    let val v = f () in
      thunk := (fn () => v); v
    end

  fun thunkFromSyntaxExp env m = ref (fn () => evalExp env m)

  (* perform pattern matching and bind variables *)
  and patEval env (Syntax.PINT n) v =
        (case force v of
              INT n' =>
                if n = n' then SOME env
                else NONE
            | _ => NONE)
    | patEval env (Syntax.PBOOL b) v =
        (case force v of
              BOOL b' =>
                if b = b' then SOME env
                else NONE
            | _ => NONE)
    | patEval env Syntax.PNIL v =
        (case force v of
              NIL => SOME env
            | _ => NONE)
    | patEval env Syntax.PWILD v = SOME env
    | patEval env (Syntax.PVAR x) v =
        SOME (StringMap.insert (env, x, v))
    | patEval env (Syntax.PCONS (p1, p2)) v =
        (case force v of
              CONS (v1, v2) =>
                (case patEval env p1 v1 of
                      NONE => NONE
                    | SOME env => patEval env p2 v2)
            | _ => NONE)
    | patEval env (Syntax.PTUPLE ps) v =
        (case force v of
              TUPLE vs =>
                ListPair.foldlEq (fn
                    (p, v, NONE) => NONE
                  | (p, v, SOME env) =>
                      patEval env p v) (SOME env) (ps, vs)
            | _ => NONE)

  and evalExp env (Syntax.INT n) = INT n
    | evalExp env (Syntax.BOOL b) = BOOL b
    | evalExp env Syntax.NIL = NIL
    | evalExp env (Syntax.VAR x) =
        force (valOf (StringMap.find (env, x)))
    | evalExp env (Syntax.ABS (x, m)) = FUN (env, x, m)
    | evalExp env (Syntax.APP (m, n)) =
        let val FUN (env0, x, m0) = evalExp env m in
          evalExp (StringMap.insert (env0, x, thunkFromSyntaxExp env n)) m0
        end
    | evalExp env (Syntax.LET (dec, m)) =
        evalExp (foldl (fn (dec, env) =>
          StringMap.unionWith #2 (env, evalDec env dec)) env dec) m
    | evalExp env (Syntax.TUPLE ms) =
        TUPLE (map (thunkFromSyntaxExp env) ms)
    | evalExp env (Syntax.CASE (m, pns)) =
        let val v = thunkFromSyntaxExp env m in
          case findMap (fn (p, n) =>
            case patEval env p v of
                 NONE => NONE
               | SOME env => SOME (evalExp env n)) pns of
               NONE => raise PatternMatchFailure
             | SOME v => v
        end
    | evalExp env (Syntax.PRIM (p, ms)) =
        (case (p, map (evalExp env) ms) of
              (Prim.PLUS, [ INT m, INT n ]) => INT (m + n)
            | (Prim.MINUS, [ INT m, INT n ]) => INT (m - n)
            | (Prim.TIMES, [ INT m, INT n ]) => INT (m * n)
            | (Prim.LE, [ INT m, INT n ]) => BOOL (m <= n))
    | evalExp env (Syntax.CONS (m, n)) =
        CONS (thunkFromSyntaxExp env m, thunkFromSyntaxExp env n)

  and evalDec env (Syntax.VAL (x, m)) =
        StringMap.singleton (x, thunkFromSyntaxExp env m)
    | evalDec env (Syntax.VALREC xms) =
        let
          val xmvs = map (fn (x, m) =>
            (x, m, ref (fn () => raise (Fail "dummy")))) xms
          val env' = foldl (fn ((x, _, v), env) =>
            StringMap.insert (env, x, v)) StringMap.empty xmvs
          val env = StringMap.unionWith #2 (env, env')
        in
          app (fn (x, m, v) => v := (fn () => evalExp env m)) xmvs;
          env
        end

  fun toString 0 _ = " ... "
    | toString _ (INT m) = Int.toString m
    | toString _ (BOOL b) = Bool.toString b
    | toString _ NIL = "[]"
    | toString _ (FUN _) = "fn"
    | toString _ (TUPLE []) = "()"
    | toString n (TUPLE (m :: ms)) =
        "("
        ^ foldr (fn (m, s) =>
            s ^ ", " ^ toString (n - 1) (force m))
            (toString (n - 1) (force m)) ms
        ^ ")"
    | toString n (CONS (m1, m2)) =
        "("
        ^ toString (n - 1) (force m1)
        ^ " :: "
        ^ toString (n - 1) (force m2)
        ^ ")"
  val toString = toString 20
end
