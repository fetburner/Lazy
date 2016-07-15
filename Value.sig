signature VALUE = sig
  type value

  val eval : Syntax.exp -> value
  val toString : value -> string
end
