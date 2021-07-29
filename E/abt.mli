(* Sort.
   The only sort in E is exp.
 *)
type sort =
  | Sexp        (* exp *)

(* Valence. PFPL notation:
    x1,...xk.a
   where a is abt argument, and x's are variables bound to a.
 *)
type valence = sort list * sort

(* Arity. PFPL notation:
    (v1,...vn)s
   where s is sort of operator, and v is valence of an argument.
   Arity of abts is generalized from that of asts (s1,...sn)s.
 *)
type arity =
  { sort:   sort
  ; params: valence list }

(* Operator.
   E has num[n], str[s], plus, times, cat, len, let.
 *)
type op =               (* PFPL notations:    *)
  | Onum of int         (*      num[n]        *)
  | Ostr of string      (*      str[s]        *)
  | Oplus               (*      plus(e1; e2)  *)
  | Otimes              (*      times(e1; e2) *)
  | Ocat                (*      cat(e1; e2)   *)
  | Olen                (*      len(e)        *)
  | Olet                (*      let(e1; x.e2) *)

(* Variable. *)
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
  | Anode of op * arg list

(* Argument.
   This type is mutual recursive with abt.
 *)
and arg = var list * abt

(* Type. *)
type typ =
  | Tnum        (* num *)
  | Tstr        (* str *)
