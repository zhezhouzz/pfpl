open Abt

let nobinding_exp = ([], Sexp)

let arity op : arity =
  match op with
  | Onum _ | Ostr _ ->
      { sort = Sexp; params = [] }
  | Oplus | Otimes | Ocat ->
      { sort = Sexp; params = [nobinding_exp; nobinding_exp] }
  | Olen ->
      { sort = Sexp; params = [nobinding_exp] }
  | Olet ->
      { sort = Sexp; params = [nobinding_exp; ([Sexp], Sexp)] }

type sort_context = (var * sort) list

(* Return sort if well-formed --- subtrees should match arity *)
let rec abt_sort (a: abt) (ctx: sort_context) : sort option =
  match a with
  | Aleaf x -> List.assoc_opt x ctx
  | Anode (op, args) ->
      let { sort; params } = arity op in
      let arg_correct (var_sorts, abt'_sort) (vars, abt') =
        List.length var_sorts = List.length vars &&
          let ctx' = List.combine vars var_sorts @ ctx in
          abt_sort abt' ctx' = Some abt'_sort
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
(* let( let(str["hello"]; x.len(x)) ; x.( x*x ) ) *)
let hello_len_square =
  let x : var = "x" in
  let e2 : abt = Anode (Otimes, [ ([], Aleaf x); ([], Aleaf x) ]) in
  Anode (Olet, [ ([], hello_len); ([x], e2) ])
(* }}} *)

(* Tests for abt_sort {{{ *)
let%test _ = abt_sort variable_x [] = None
let%test _ = abt_sort q2 [] = Some Sexp
let%test _ = abt_sort hello_len [] = Some Sexp
let%test _ = abt_sort hello_len_square [] = Some Sexp
(* }}} *)

type typ =
  | Tnum        (* num *)
  | Tstr        (* str *)

type typ_context = (var * typ) list

type exp =
  | Evar of var
  | Enum of int
  | Estr of string
  | Eplus of exp * exp
  | Etimes of exp * exp
  | Ecat of exp * exp
  | Elen of exp
  | Elet of exp * var * exp

let (let*) = Option.bind

let rec exp_from_abt (a: abt) (ctx: sort_context) : exp option =
  match a with
  | Aleaf x ->
      let* sort = List.assoc_opt x ctx in
      if sort = Sexp then Some (Evar x) else None
  | Anode (op, args) ->
      match op, args with
      | Onum n, [] -> Some (Enum n)
      | Ostr s, [] -> Some (Estr s)
      | Oplus, [([], a1); ([], a2)] ->
          let* e1 = exp_from_abt a1 ctx in
          let* e2 = exp_from_abt a2 ctx in
          Some (Eplus (e1, e2))
      | Otimes, [([], a1); ([], a2)] ->
          let* e1 = exp_from_abt a1 ctx in
          let* e2 = exp_from_abt a2 ctx in
          Some (Etimes (e1, e2))
      |  Ocat, [([], a1); ([], a2)] ->
          let* e1 = exp_from_abt a1 ctx in
          let* e2 = exp_from_abt a2 ctx in
          Some ( Ecat (e1, e2))
      | Olen, [([], a1)] ->
          let* e1 = exp_from_abt a1 ctx in
          Some (Elen e1)
      | Olet, [([], a1); ([x], a2)] ->
          let* e1 = exp_from_abt a1 ctx in
          let ctx' = (x, Sexp) :: ctx in
          let* e2 = exp_from_abt a2 ctx' in
          Some (Elet (e1, x, e2))
      | (Onum _ | Ostr _ | Oplus | Otimes | Ocat | Olen | Olet), _ ->
          None

let rec exp_to_abt (e: exp) : abt =
  match e with
  | Evar x           -> Aleaf x
  | Enum n           -> Anode (Onum n, [])
  | Estr s           -> Anode (Ostr s, [])
  | Eplus (e1, e2)   -> Anode (Oplus, [([], exp_to_abt e1); ([], exp_to_abt e2)])
  | Etimes (e1, e2)  -> Anode (Otimes, [([], exp_to_abt e1); ([], exp_to_abt e2)])
  | Ecat (e1, e2)    -> Anode (Ocat, [([], exp_to_abt e1); ([], exp_to_abt e2)])
  | Elen e1          -> Anode (Olen, [([], exp_to_abt e1)])
  | Elet (e1, x, e2) -> Anode (Olet, [([], exp_to_abt e1); ([x], exp_to_abt e2)])

(* Return type if well-typed *)
let rec exp_typ (e: exp) (ctx: typ_context) : typ option =
  match e with
  | Evar x -> List.assoc_opt x ctx
  | Enum _ -> Some Tnum
  | Estr _ -> Some Tstr
  | Eplus (e1, e2) | Etimes (e1, e2) ->
      let* typ1 = exp_typ e1 ctx in
      let* typ2 = exp_typ e2 ctx in
      if typ1 = Tnum && typ2 = Tnum then Some Tnum else None
  | Ecat (e1, e2) ->
      let* typ1 = exp_typ e1 ctx in
      let* typ2 = exp_typ e2 ctx in
      if typ1 = Tnum && typ2 = Tnum then Some Tnum else None
  | Elen e1 ->
      let* typ1 = exp_typ e1 ctx in
      if typ1 = Tstr then Some Tnum else None
  | Elet (e1, x, e2) ->
      let* typ1 = exp_typ e1 ctx in
      let ctx' = (x, typ1) :: ctx in
      let* typ2 = exp_typ e2 ctx' in
      Some typ2

(* Tests for exp_from_abt {{{ *)
let%test _ =
  None = let* e = exp_from_abt variable_x [] in exp_typ e []
let%test _ =
  None = let* e = exp_from_abt q2 [] in exp_typ e []
let%test _ =
  Some Tnum = let* e = exp_from_abt hello_len [] in exp_typ e []
let%test _ =
  Some Tnum = let* e = exp_from_abt hello_len_square [] in exp_typ e []
(* }}} *)

let subst (b: exp) (x: var) (a: exp) : exp option =
  let abtb = exp_to_abt b in
  let abta = exp_to_abt a in
  let* abt = Abt.subst abtb x abta in
  exp_from_abt abt []

(* Tests for subst {{{ *)
let%test _ =
  let e1 = Enum 0 in
  let x, y = "x", "y" in
  let e2 = Eplus (Evar x, Evar y) in
  subst (Evar y) x (Elet (e1, x, e2)) = None

let%test _ =
  let x = "x" in
  subst (Elen (Estr "hello")) x (Eplus (Evar x, Enum 1)) =
    Some (Eplus (Elen (Estr "hello"), Enum 1))
(* }}} *)
