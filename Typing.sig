signature TYPING = sig
  exception UnboundVar of string
  val typing : Syntax.exp -> Type.typ
end
