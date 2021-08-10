open Statics

(* Closed values, the final states in the transition system.
   PFPL notation:
    e val
 *)
val valu : exp -> bool

(* One step of the transition. PFPL notation:
    e |---> e'
   It's a by-value interpretation.
   Maps to None when stuck or final (val).
 *)
val transition : exp -> exp option

(* State where the system cannot transit from
*)
val normalize : exp -> exp
