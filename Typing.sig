signature TYPING = sig
  exception UnboundVar of string
  val typingDec : Type.scheme StringMap.map -> Syntax.dec -> Type.scheme StringMap.map 
end
