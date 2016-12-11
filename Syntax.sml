structure Syntax = struct
  datatype pat =
    (* constructor *)
      PCONS of Cons.cons * pat list
    (* _ *)
    | PWILD
    (* x *)
    | PVAR of string

  (* abstract syntax tree of expression *)
  datatype exp =
    (* consructor *)
      CONS of Cons.cons * exp list
    (* variable *)
    | VAR of string
    (* fn x => M *)
    | ABS of string * exp
    (* M N *)
    | APP of exp * exp
    (* let d in N end *)
    | LET of dec list * exp
    (* case M of p1 => N1 | ... | pn => Nn *)
    | CASE of exp * (pat * exp) list
    (* primitives *)
    | PRIM of Prim.prim * exp list
  (* abstract syntax tree of declaration *)
  and dec =
    (* val x = M *)
      VAL of string * exp
    (* val rec x1 = M1 and ... and xn = Mn *)
    | VALREC of (string * exp) list
end
