structure TypeCons = struct
  datatype tycons =
      INT
    | BOOL
    | TUPLE
    | LIST
    | ARROW

  fun toString INT [] = "int"
    | toString BOOL [] = "bool"
    | toString TUPLE [] = "unit"
    | toString TUPLE (s :: ss) =
        "(" ^ foldl (fn (t, s) => s ^ " * " ^ t) s ss ^ ")"
    | toString LIST [s] = s ^ " list"
    | toString ARROW [s1, s2] =
        "(" ^ s1 ^ " -> " ^ s2 ^ ")"

  val equal = op=
end
