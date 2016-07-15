structure Value : VALUE = struct
  datatype value =
      INT of int
    | BOOL of bool
    | FUN of thunk StringMap.map * string * Syntax.exp
    | RECFUN of thunk StringMap.map * string * string * Syntax.exp
    | TUPLE of thunk list
  and thunk_body =
      VALUE of value
    | SUSPEND of thunk StringMap.map * Syntax.exp
  withtype thunk = thunk_body ref

  local 
    fun force (ref (VALUE v)) = v
      | force (r as ref (SUSPEND (env, m))) =
          let val v = eval env m in
            r := VALUE v; v
          end

    and eval env (Syntax.INT n) = INT n
      | eval env (Syntax.BOOL b) = BOOL b
      | eval env (Syntax.VAR x) =
          force (valOf (StringMap.find (env, x)))
      | eval env (Syntax.IF (m, n1, n2)) =
          (case eval env m of
                BOOL true => eval env n1
              | BOOL false => eval env n2)
      | eval env (Syntax.ABS (x, m)) = FUN (env, x, m)
      | eval env (Syntax.APP (m, n)) =
          (case eval env m of
                FUN (env0, x, m0) =>
                  eval (StringMap.insert (env0, x, ref (SUSPEND (env, n)))) m0
              | v as RECFUN (env0, f, x, m0) =>
                  eval
                    (StringMap.insert
                      (StringMap.insert (env0, f, ref (VALUE v)),
                       x,
                       ref (SUSPEND (env, n)))) m0)
      | eval env (Syntax.LET (dec, m)) =
          eval (foldl (fn
              (Syntax.VAL (x, m), env) =>
                StringMap.insert (env, x, ref (SUSPEND (env, m)))
            | (Syntax.VALREC (f, x, m), env) =>
                StringMap.insert (env, f, ref (VALUE (RECFUN (env, f, x, m))))) env dec) m
      | eval env (Syntax.TUPLE ms) =
          TUPLE (map (fn m => ref (SUSPEND (env, m))) ms)
      | eval env (Syntax.LET_TUPLE (m, xs, n)) =
          let val TUPLE ms = eval env m in
            eval (ListPair.foldlEq (fn (x, m, env) =>
              StringMap.insert (env, x, m)) env (xs, ms)) n
          end
      | eval env (Syntax.PLUS (m, n)) =
          let
            val INT m = eval env m
            val INT n = eval env n
          in
            INT (m + n)
          end
      | eval env (Syntax.MINUS (m, n)) =
          let
            val INT m = eval env m
            val INT n = eval env n
          in
            INT (m - n)
          end
      | eval env (Syntax.TIMES (m, n)) =
          let
            val INT m = eval env m
            val INT n = eval env n
          in
            INT (m * n)
          end
      | eval env (Syntax.LE (m, n)) =
          let
            val INT m = eval env m
            val INT n = eval env n
          in
            BOOL (m <= n)
          end
  in
    val eval = eval StringMap.empty

    fun toString 0 _ = " ... "
      | toString _ (INT m) = Int.toString m
      | toString _ (BOOL b) = Bool.toString b
      | toString _ (FUN _) = "fn"
      | toString _ (RECFUN _) = "fn"
      | toString _ (TUPLE []) = "()"
      | toString n (TUPLE (m :: ms)) =
          "("
          ^ foldr (fn (m, s) =>
              s ^ ", " ^ toString (n - 1) (force m))
              (toString (n - 1) (force m)) ms
          ^ ")"
    val toString = toString 3
  end
end
