signature TYPE =
sig
  type typ
  type scheme

  val INT : typ
  val BOOL : typ
  val ARROW : typ * typ -> typ
  val LIST : typ -> typ
  val TUPLE : typ list -> typ

  val toString : typ -> string
  val schemeToString : scheme -> string
  val toTypeScheme : typ -> scheme

  val genvar : int -> typ
  val inst : int -> scheme -> typ * typ IdMap.map
  val generalize : int -> typ -> scheme * IdSet.set

  exception Unify of typ * typ

  val unify : typ * typ -> unit
end
