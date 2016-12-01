structure Main = struct
  structure LazyLrVals = LazyLrValsFun(structure Token = LrParser.Token)
  structure LazyLex = LazyLexFun(structure Tokens = LazyLrVals.Tokens)
  structure LazyParser = Join(structure LrParser = LrParser
                             structure ParserData = LazyLrVals.ParserData
                             structure Lex = LazyLex)

  fun exec dec (env, tenv) =
    let
      val tenv' = Typing.typingDec tenv dec
      val env' = Value.evalDec env dec
    in
      StringMap.intersectWithi (fn (x, v, t) =>
        print
          ("val " ^ x ^ " = "
           ^ Value.toString (Value.force v)
           ^ " : "
           ^ Type.schemeToString t
           ^ "\n")) (env', tenv');
        (StringMap.unionWith #2 (env, env'), StringMap.unionWith #2 (tenv, tenv'))
    end
    handle
      Type.Unify (t1, t2) =>
        (print
          ("Error : failed to unify "
            ^ Type.toString t1
            ^ " and "
            ^ Type.toString t2); (env, tenv))
    | Value.Bottom =>
        (print "Error : infinite loop"; (env, tenv))
    | Value.PatternMatchFailure =>
        (print "Error : pattern matching failed"; (env, tenv))
    | Typing.UnboundVar x =>
        (print ("Error : unbound variable " ^ x); (env, tenv))
    | ListPair.UnequalLengths =>
        (print "Error : inconsistent arity"; (env, tenv))

  fun print_error (s, _, _) = (print s; print "\n")
  (* REPL *)
  fun readEvalPrintLoop stat lexer =
    let
      val () = print "\n- "
      val (result, lexer') = LazyParser.parse (0, lexer, print_error, ())
      val stat' = exec result stat
      val (next, lexer'') = LazyParser.Stream.get lexer'
    in
      readEvalPrintLoop stat' lexer''
    end
  val lexer = LazyParser.makeLexer (fn _ => valOf (TextIO.inputLine TextIO.stdIn))
  fun run (cmd, args) = (readEvalPrintLoop (StringMap.empty, StringMap.empty) lexer; OS.Process.success)
end
