structure Cons = struct
  (* constructor *)
  datatype cons =
      INT of int
    | BOOL of bool
    | NIL of Type.typ
    | CONS of Type.typ
    | TUPLE of Type.typ list

  fun dom (INT _) = []
    | dom (BOOL _) = []
    | dom (NIL _) = []
    | dom (CONS t) = [ t, Type.LIST t ]
    | dom (TUPLE ts) = ts

  fun cod (INT _) = Type.INT
    | cod (BOOL _) = Type.BOOL
    | cod (NIL t) = Type.LIST t
    | cod (CONS t) = Type.LIST t
    | cod (TUPLE ts) = Type.TUPLE ts

  fun equal (INT n, INT n') = n = n'
    | equal (BOOL b, BOOL b') = b = b'
    | equal (NIL _, NIL _) = true
    | equal (CONS _, CONS _) = true
    | equal (TUPLE _, TUPLE _) = true
    | equal _ = false
end
