open Ast
open Printf

let ast_to_string ast =
  let rec aux = function
    | Var x -> x
    | Fun (x, body) -> sprintf "(fun %s -> %s)" x (aux_t body)
    | App (x, y) -> sprintf "(%s %s)" (aux_t x) (aux_t y)
  and aux_t (_, tm) = aux tm
  in
  aux_t ast

let subst ast name (_, target) : t =
  let rec aux = function
    | Var x when String.equal x name -> target
    | Var x -> Var x
    | Fun (x, body) when String.equal x name -> Fun (x, body)
    | Fun (x, body) -> Fun (x, aux_t body)
    | App (x, y) -> App (aux_t x, aux_t y)
  and aux_t (loc, tm) = loc, aux tm
  in
  aux_t ast

let do_app tm (y:t) : t=
  let (_, x) = tm in
  match x with
  | Var x -> failwith (sprintf "not a function: %s" x)
  | Fun (x', body) -> subst body x' y
  | App (_, _) -> failwith (sprintf "not a function: %s" (ast_to_string tm))

let call_by_value term =
  let rec aux (loc, term) =
    match term with
    | Var _ -> None
    | Fun (_, _) -> None
    | App (x, y) ->
      match aux y with
      | Some y' -> Some (loc, App (x, y'))
      | None ->
        match aux x with
        | Some x' -> Some (loc, App (x', y))
        | None -> Some (do_app x y)
  in
  aux term

let call_by_name term =
  let rec aux (loc, term) =
    match term with
    | Var _ -> None
    | Fun (_, _) -> None
    | App (x, y) ->
      match aux x with
      | Some x' -> Some (loc, App (x', y))
      | None ->
        match aux y with
        | Some y' -> Some (loc, App (x, y'))
        | None -> Some (do_app x y)
  in
  aux term

let small_step_loop strategy ast =
  let f =
    match strategy with
    | "cbn" -> call_by_name
    | "cbv" -> call_by_value
    | _ -> failwith (sprintf "unknown strategy: %s" strategy)
  in
  let counter = ref 0 in
  let rec aux ast =
    let () = printf "%i: %s\n" (!counter) (ast_to_string ast) in
    match f ast with
    | None -> printf "end\n"
    | Some ast' -> counter := !counter + 1; aux ast'
  in
  aux ast

