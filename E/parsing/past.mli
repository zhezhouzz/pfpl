type loc = Lexing.position
and opt = loc * Abt.op
type var = string
(* Abstract binding tree. PFPL notation:
    a ::=
        | x
   where x is a variable, x1
        | o(x1.a1;...xn.an)
   where x is a sequence of variables, a is a sub-abt.
*)

type abt =
  | Aleaf of var
  | Anode of opt * (var list * abtt) list
and abtt = loc * abt
