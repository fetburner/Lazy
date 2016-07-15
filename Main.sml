structure Main = struct
  structure LazyLrVals = LazyLrValsFun(structure Token = LrParser.Token)
  structure LazyLex = LazyLexFun(structure Tokens = LazyLrVals.Tokens)
  structure LazyParser = Join(structure LrParser = LrParser
                             structure ParserData = LazyLrVals.ParserData
                             structure Lex = LazyLex)

  fun exec exp stat =
    ((let
      val t = Typing.typing exp
      val v = Value.eval exp
    in
      print
        ("val it = "
         ^ Value.toString v
         ^ " : "
         ^ Type.toString t)
    end
    handle
      Type.Unify (t1, t2) =>
        print
          ("Error : failed to unify "
            ^ Type.toString t1
            ^ " and "
            ^ Type.toString t2)
    | Typing.UnboundVar x =>
       print ("Error : unbound variable " ^ x)
    | ListPair.UnequalLengths =>
        print "Error : inconsistent arity");
    print "\n- ";
    stat)

  fun print_error (s, _, _) = (print s; print "\n")
  (* REPL *)
  fun readEvalPrintLoop stat lexer =
    let
      val (result, lexer') = LazyParser.parse(0, lexer, print_error, ())
          val stat' = exec result stat
          val (next, lexer'') = LazyParser.Stream.get lexer'
    in
      readEvalPrintLoop stat' lexer''
    end
  val lexer = LazyParser.makeLexer (fn _ => valOf (TextIO.inputLine TextIO.stdIn))
  fun run (cmd, args) = (readEvalPrintLoop () lexer; OS.Process.success)

end
