(**
  * E, the simple expression language.
  * Defined in Statics, PFPL:Ch4.
  *)

(* Sort.
   The only sort in E is exp. 
 *)
type sort =
  | Sexp

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
type arity = {
  sort:   sort;
  params: valence list;
}

(* Operations in E are 
   num[n], str[s], plus, times, cat, len, let.
 *)
type op =               (* PFPL notations:    *)
  | Onum of int         (*      num[n]        *)
  | Ostr of string      (*      str[s]        *)
  | Oplus               (*      plus(e1; e2)  *)
  | Otimes              (*      times(e1; e2) *)
  | Ocat                (*      cat(e1; e2)   *)
  | Olen                (*      len(e)        *)
  | Olet                (*      let(e1; x.e2) *)

let nullary_exp = ([], Sexp)
(* Op arguments:
     num[n], str[s]:   []
     plus, times, cat: [exp, exp]
     len:              [exp]
     let:              [exp, [exp].exp]
 *)
let arity op : arity =
  match op with
  | Onum _ | Ostr _       -> { sort = Sexp; params = [] }
  | Oplus | Otimes | Ocat -> { sort = Sexp; params = [nullary_exp; nullary_exp] }
  | Olen                  -> { sort = Sexp; params = [nullary_exp] }
  | Olet                  -> { sort = Sexp; params = [nullary_exp; ([Sexp], Sexp)] }

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
  | Anode of op * (var list * abt) list

type sort_binding = (var * sort) list
(* Return sort if well-formed --- subtrees should match arity *)
let rec abt_sort (b: sort_binding) (a: abt) : sort option =
  match a with
  | Aleaf x -> List.assoc_opt x b
  | Anode (op, args) -> 
      let { sort; params } = arity op in
      let arg_correct (var_sorts, abt'_sort) (vars, abt') =
        List.length var_sorts = List.length vars &&
          let b' = List.combine vars var_sorts @ b in
          abt_sort b' abt' = Some abt'_sort
      in
      if List.for_all2 arg_correct params args
      then Some sort
      else None

(* Examples {{{ *)
(* x *)
let variable_x = Aleaf "x"
(* times(str["q"], num[2]) *)
let q2 = Anode (
  Otimes, 
  [ ([], Anode (Ostr "q", [])); 
    ([], Anode (Onum 2, [])) ])
(* let(str["hello"]; x.len(x)) *)
let hello_len = 
  let e1 : abt = Anode (Ostr "hello", []) in
  let x : var = "x" in
  let e2 : abt = Anode (Olen, [ ([], Aleaf x) ]) in 
  Anode (Olet, [ ([], e1); ([x], e2) ])
(* }}} *)

(* Substitution. *)
let rec subst (b: abt) (x: var) (a: abt) : abt =
  match a with
  | Aleaf x' -> if x = x' then b else a
  | Anode (op, args) ->
      let args' = [] in  (* TODO *)
      Anode (op, args')

(* Type. *)
type typ =
  | Tnum
  | Tstr



(* Return type if well-typed ---  *)
