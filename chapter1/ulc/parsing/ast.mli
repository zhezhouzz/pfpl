type loc = Lexing.position
type tm =
  | Var of string
  | Fun of string * t
  | App of t * t
and t = loc * tm
