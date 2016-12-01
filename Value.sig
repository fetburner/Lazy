signature VALUE = sig
  type value
  type thunk

  val evalDec : thunk StringMap.map -> Syntax.dec -> thunk StringMap.map
  val force : thunk -> value
  val toString : value -> string

  exception Bottom
  exception PatternMatchFailure
end
