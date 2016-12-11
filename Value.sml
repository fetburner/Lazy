structure Value : VALUE = struct
  exception Bottom
  exception PatternMatchFailure

  datatype value =
      CONS of Cons.cons * thunk list
    (* closure *)
    | FUN of env * string * Syntax.exp
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
    let
      val () = thunk := (fn () => raise Bottom)
      val v = f ()
      val () = thunk := (fn () => v)
    in v end

  (* perform pattern matching and bind variables *)
  fun patEval env (Syntax.PCONS (c, ps)) v =
        (case force v of
              CONS (c', vs) =>
                if Cons.equal (c, c') then
                  ListPair.foldlEq (fn
                      (p, v, NONE) => NONE
                    | (p, v, SOME env) =>
                        patEval env p v) (SOME env) (ps, vs)
                else NONE
            | _ => NONE)
    | patEval env Syntax.PWILD v = SOME env
    | patEval env (Syntax.PVAR x) v =
        SOME (StringMap.insert (env, x, v))

  fun thunkFromSyntaxExp env m = ref (fn () => evalExp env m)

  and evalExp env (Syntax.CONS (c, ms)) =
        CONS (c, map (thunkFromSyntaxExp env) ms)
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
              (Prim.PLUS, [ CONS (Cons.INT m, []), CONS (Cons.INT n, []) ]) =>
                CONS (Cons.INT (m + n), [])
            | (Prim.MINUS, [ CONS (Cons.INT m, []), CONS (Cons.INT n, []) ]) =>
                CONS (Cons.INT (m - n), [])
            | (Prim.TIMES, [ CONS (Cons.INT m, []), CONS (Cons.INT n, []) ]) =>
                CONS (Cons.INT (m * n), [])
            | (Prim.LE, [ CONS (Cons.INT m, []), CONS (Cons.INT n, []) ]) =>
                CONS (Cons.BOOL (m <= n), []))

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
    | toString _ (CONS (Cons.INT m, [])) = Int.toString m
    | toString _ (CONS (Cons.BOOL b, [])) = Bool.toString b
    | toString _ (CONS (Cons.NIL _, [])) = "[]"
    | toString _ (FUN _) = "fn"
    | toString _ (CONS (Cons.TUPLE _, [])) = "()"
    | toString n (CONS (Cons.TUPLE _, (m :: ms))) =
        "("
        ^ foldr (fn (m, s) =>
            s ^ ", " ^ toString (n - 1) (force m))
            (toString (n - 1) (force m)) ms
        ^ ")"
    | toString n (CONS (Cons.CONS _, [ m1, m2 ])) =
        "("
        ^ toString (n - 1) (force m1)
        ^ " :: "
        ^ toString (n - 1) (force m2)
        ^ ")"
  val toString = toString 20
end
