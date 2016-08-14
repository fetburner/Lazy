signature TYPE =
sig
  type tvar
  datatype typ =
    (* type variable *)
      VAR of tvar ref
    (* quantified type variable *)
    | META of Id.id
    | INT
    | BOOL
    | ARROW of typ * typ
    | TUPLE of typ list
    | LIST of typ
  type scheme

  val toString : typ -> string
  val schemeToString : scheme -> string
  val toTypeScheme : typ -> scheme

  val genvar : int -> typ
  val inst : int -> scheme -> typ * typ IdMap.map
  val generalize : int -> typ -> scheme * IdSet.set

  exception Unify of typ * typ

  val unify : typ * typ -> unit
end
