(**
  * E, the simple expression language.
  * Defined in Statics, PFPL:Ch4.
  *)

(* The only sort in E is exp. *)
type sort =
  | Sexp

type valence = sort list * sort

type arity = {
  sort:   sort;
  params: valence list;
}

let nullary_exp = ([], Sexp)

(* Operations in E are num[n], str[s], plus, times, cat, len, let.
   Their arguments:
     num[n], str[s]:   []
     plus, times, cat: [exp, exp]
     len:              [exp]
     let:              [exp, [exp].exp]
 *)
type op =
  | Onum of int | Ostr of string
  | Oplus | Otimes | Ocat
  | Olen
  | Olet

let arity op : arity =
  match op with
  | Onum _ | Ostr _       -> { sort = Sexp; params = [] }
  | Oplus | Otimes | Ocat -> { sort = Sexp; params = [nullary_exp; nullary_exp] }
  | Olen                  -> { sort = Sexp; params = [nullary_exp] }
  | Olet                  -> { sort = Sexp; params = [nullary_exp; ([Sexp], Sexp)] }

type var = string

type abt =
  | Aleaf of var
  | Anode of op * (var list * abt) list

type binding = (var * sort) list

(* Return sort if well-formed --- subtrees should match arity *)
let rec abt_sort (b: binding) (a: abt) : sort option =
  match a with
  | Aleaf x -> List.assoc_opt x b
  | Anode (op, args) -> 
      let open List in
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

let rec subst (b: abt) (x: var) (a: abt) : abt =
  match a with
  | Aleaf x' -> if x = x' then b else a
  | Anode (op, args) ->
      let args' = [] in
      Anode (op, args')

type typ =
  | Tnum
  | Tstr



(* Return type if well-typed ---  *)
