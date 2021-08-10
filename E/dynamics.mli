open Statics

(* Closed values, the final states in the transition system.
   PFPL notation:
    e val
 *)
val valu : exp -> bool

(* One step of the transition. PFPL notation:
    e |---> e'
   It's a by-value interpretation.
 *)
val transition : exp -> exp option
