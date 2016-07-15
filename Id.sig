signature ID =
sig
  type id
  val gensym : unit -> id
  val toString : id -> string
  val compare : id * id -> order
end
