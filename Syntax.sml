structure Syntax = struct
  datatype pat =
    (* constant *)
      PINT of int
    | PBOOL of bool
    | PNIL
    (* x *)
    | PVAR of string
    (* p :: p *)
    | PCONS of pat * pat
    (* (p1, ... , pn) *)
    | PTUPLE of pat list

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
    (* case M of p1 => N1 | ... | pn => Nn *)
    | CASE of exp * (pat * exp) list
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
end
