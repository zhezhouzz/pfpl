open Abt

(* Sort context. *)
type sort_context

(* A well-formed abt has a sort,
   arguments must match valence for each node
 *)
val abt_sort : abt -> sort_context -> sort option

(* Expression. PFPL notation:
    e
 *)
type exp =
  | Evar of var
  | Enum of int
  | Estr of string
  | Eplus of exp * exp
  | Etimes of exp * exp
  | Ecat of exp * exp
  | Elen of exp
  | Elet of exp * var * exp

(* If the abt is an exp, i.e., sort is Sexp,
   then return exp
 *)
val exp_from_abt : abt -> sort_context -> exp option

val exp_to_abt : exp -> abt

(* Type. *)
type typ =
  | Tnum        (* num *)
  | Tstr        (* str *)

(* Typing context. PFPL notation:
    Γ
 *)
type typ_context

(* A well-typed abt of sort exp, i.e., an expression, has a type,
   by rules defining the statics of E (Eq:4.1).
   PFPL notation:
     Γ ⊢ e : τ
   where Γ is a typing context, e an exp, τ a type.
 *)
val exp_typ : exp -> typ_context -> typ option

(* Substitution of an exp abt.
   See Abt.subst
 *)
val subst : exp -> var -> exp -> exp option
