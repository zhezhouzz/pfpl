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

(* https://ocaml.org/manual/bindingops.html *)
let ( let* ) o f =
  match o with
  | None -> None
  | Some x -> f x

let return x = Some x

type typ_context = (var * typ) list

let rec abt_typ (a: abt) (ctx: typ_context) : typ option =
  match a with
  | Aleaf x -> List.assoc_opt x ctx
  | Anode (op, args) ->
      match op, args with
      | Onum _, [] -> Some Tnum
      | Ostr _, [] -> Some Tstr
      | Oplus, [([], a1); ([], a2)] | Otimes, [([], a1); ([], a2)] -> 
          let* typ1 = abt_typ a1 ctx in
          let* typ2 = abt_typ a2 ctx in
          if typ1 = Tnum && typ2 = Tnum then return Tnum else None
      | Ocat, [([], a1); ([], a2)] ->
          let* typ1 = abt_typ a1 ctx in
          let* typ2 = abt_typ a2 ctx in
          if typ1 = Tstr && typ2 = Tstr then return Tstr else None
      | Olen, [([], a1)] -> 
          let* typ1 = abt_typ a1 ctx in
          if typ1 = Tstr then return Tnum else None
      | Olet, [([], a1); ([x], a2)] ->
          let* typ1 = abt_typ a1 ctx in
          let ctx' = (x, typ1) :: ctx in
          let* typ2 = abt_typ a2 ctx' in
          return typ2
      | (Onum _ | Ostr _ | Oplus | Otimes | Ocat | Olen | Olet), _ -> 
          None (* a is not well-formed *)
