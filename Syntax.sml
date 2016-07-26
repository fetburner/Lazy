structure Syntax = struct
  (* abstract syntax tree of expression *)
  datatype exp =
    (* constant *)
      INT of int
    | BOOL of bool
    | NIL
    (* variable *)
    | VAR of string
    (* if M then N_1 else N_2 *)
    | IF of exp * exp * exp
    (* fn x => M *)
    | ABS of string * exp
    (* M N *)
    | APP of exp * exp
    (* let d in N end *)
    | LET of dec list * exp
    (* (M_1, ... , M_n) *)
    | TUPLE of exp list
    (* case M of (x_1, ..., x_n) N *)
    | LET_TUPLE of exp * string list * exp
    (* case M of [] => N1 | x :: y => N2 *)
    | CASE of exp * exp * string * string * exp
    (* primitives *)
    | PLUS of exp * exp
    | MINUS of exp * exp
    | TIMES of exp * exp
    | LE of exp * exp
    | CONS of exp * exp
  (* abstract syntax tree of declaration *)
  and dec =
    (* val x = M *)
      VAL of string * exp
    (* val rec x1 = M1 and ... and xn = Mn *)
    | VALREC of (string * exp) list

  fun expToString (INT n) = Int.toString n
    | expToString (BOOL b) = Bool.toString b
    | expToString NIL = "[]"
    | expToString (VAR x) = x
    | expToString (IF (m, n1, n2)) =
        "(if "
        ^ expToString m
        ^ " then "
        ^ expToString n1
        ^ " else "
        ^ expToString n2
        ^ ")"
    | expToString (ABS (x, m)) =
        "(fn "
        ^ x
        ^ " => "
        ^ expToString m
        ^ ")"
    | expToString (APP (m, n)) =
        "("
        ^ expToString m
        ^ " "
        ^ expToString n
        ^ ")"
    | expToString (LET (dec, m)) =
        "let "
        ^ (case dec of
                [] => ""
              | d :: dec =>
                  foldr (fn (d, s) =>
                    decToString d ^ s) (decToString d) dec)
        ^ " in "
        ^ expToString m
        ^ " end"
    | expToString (TUPLE []) = "()"
    | expToString (TUPLE (m :: ms)) =
       foldr (fn (m, s) => expToString m ^ s) (expToString m) ms
    | expToString (LET_TUPLE (m, xs, n)) =
        "(case "
        ^ expToString m
        ^ " of "
        ^ (case xs of
                [] => ""
              | x :: xs =>
                  foldr op^ x xs)
        ^ " => "
        ^ expToString n
        ^ ")"
    | expToString (CASE (m, n1, x, y, n2)) =
        "(case "
        ^ expToString m
        ^ " of [] => "
        ^ expToString n1
        ^ " | "
        ^ x
        ^ " :: "
        ^ y
        ^ " => "
        ^ expToString n2
        ^ ")"
    | expToString (PLUS (m, n)) =
        "("
        ^ expToString m
        ^ " + "
        ^ expToString n
        ^ ")"
    | expToString (MINUS (m, n)) =
        "("
        ^ expToString m
        ^ " - "
        ^ expToString n
        ^ ")"
    | expToString (TIMES (m, n)) =
        "("
        ^ expToString m
        ^ " - "
        ^ expToString n
        ^ ")"
    | expToString (LE (m, n)) =
        "("
        ^ expToString m
        ^ " <= "
        ^ expToString n
        ^ ")"
    | expToString (CONS (m, n)) =
        "("
        ^ expToString m
        ^ " :: "
        ^ expToString n
        ^ ")"
  and decToString (VAL (x, m)) =
        "val "
        ^ x
        ^ " = "
        ^ expToString m
    | decToString (VALREC []) = ""
    | decToString (VALREC [(x, m)]) =
        "val rec "
        ^ x
        ^ " = "
        ^ expToString m
    | decToString (VALREC ((x, m) :: xms)) =
        "val rec "
        ^ x
        ^ " = "
        ^ expToString m
        ^ foldl (fn ((x, m), s) =>
            s
            ^ " and "
            ^ x
            ^ " = " 
            ^ expToString m) "" xms
end
