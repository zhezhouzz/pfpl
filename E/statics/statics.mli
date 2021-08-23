open Abt

(* Sort context. *)
type sort_context

(* A well-formed abt has a sort,
   arguments must match valence for each node
 *)
val abt_sort : abt -> sort_context -> sort option

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
val abt_typ : abt -> typ_context -> typ option
