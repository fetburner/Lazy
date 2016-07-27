structure Prim = struct
  datatype prim = PLUS | MINUS | TIMES | LE

  fun dom PLUS = [ Type.INT, Type.INT ]
    | dom MINUS = [ Type.INT, Type.INT ]
    | dom TIMES = [ Type.INT, Type.INT ]
    | dom LE = [ Type.INT, Type.INT ]

  fun cod PLUS = Type.INT
    | cod MINUS = Type.INT
    | cod TIMES = Type.INT
    | cod LE = Type.BOOL
end
