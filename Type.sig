signature TYPE =
sig
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
  and tvar = UNBOUND of Id.id * int | LINK of typ

  type scheme

  val toString : typ -> string
  val toTypeScheme : typ -> scheme

  val genvar : int -> typ
	val inst : int -> scheme -> typ
	val generalize : int -> typ -> scheme

  exception Unify of typ * typ

	val unify : typ * typ -> unit
end
