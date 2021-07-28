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

type sort_binding = (var * sort) list

(* Return sort if well-formed --- subtrees should match arity *)
let rec abt_sort (a: abt) (b: sort_binding) : sort option =
  match a with
  | Aleaf x -> List.assoc_opt x b
  | Anode (op, args) -> 
      let { sort; params } = arity op in
      let arg_correct (var_sorts, abt'_sort) (vars, abt') =
        List.length var_sorts = List.length vars &&
          let b' = List.combine vars var_sorts @ b in
          abt_sort abt' b' = Some abt'_sort
      in
      if List.for_all2 arg_correct params args
      then Some sort
      else None
