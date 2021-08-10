type sort =
  | Sexp        (* exp *)

type valence = sort list * sort

type arity =
  { sort:   sort
  ; params: valence list }

type op =               (* PFPL notations:    *)
  | Onum of int         (*      num[n]        *)
  | Ostr of string      (*      str[s]        *)
  | Oplus               (*      plus(e1; e2)  *)
  | Otimes              (*      times(e1; e2) *)
  | Ocat                (*      cat(e1; e2)   *)
  | Olen                (*      len(e)        *)
  | Olet                (*      let(e1; x.e2) *)

type var = string

type abt =
  | Aleaf of var
  | Anode of op * arg list

and arg = var list * abt

(* Maps to true if variable x is in abt a *)
let rec mem (a: abt) (x: var) : bool =
  match a with
  | Aleaf x' -> x = x'
  | Anode (_, args) ->
      let mem_arg (vars, a1) =
        List.for_all ((!=) x) vars &&
        mem a1 x
      in
      List.exists mem_arg args

let (let*) = Option.bind

let rec subst (b: abt) (x: var) (a: abt) : abt option =
  match a with
  | Aleaf x' ->
      Some (if x = x' then b else a)
  | Anode (op, args) ->
      let undefined = args
        |> List.map fst
        |> List.concat
        |> List.exists (mem b)
      in
      let subst_arg (vars, a1) : arg option =
        if List.mem x vars then
          Some (vars, a1)
        else
          let* a1' = subst b x a1 in
          Some (vars, a1')
      in
      let arg_opts = List.map subst_arg args in
      let args_opt =
        List.fold_right
          (fun arg_opt acc ->
            match arg_opt, acc with
            | Some arg, Some lst -> Some (arg :: lst)
            | _                  -> None
          ) arg_opts (Some [])
      in
      match undefined, args_opt with
      | false, Some args' -> Some (Anode (op, args'))
      | _                 -> None

