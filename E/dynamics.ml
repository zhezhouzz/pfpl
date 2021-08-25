open Statics

let (let*) = Option.bind

let valu (e: exp) : bool =
  match e with
  | Enum _              (* 5.3a *)
  | Estr _ -> true      (* 5.3b *)
  | Evar _ | Eplus _ | Etimes _ | Ecat _ | Elen _ | Elet _ -> false

let rec transition (e: exp) : exp option =
  match e with
  | Enum _ | Estr _ -> None
  | Eplus (Enum n1, Enum n2) ->
      Some (Enum (n1 + n2))                             (* 5.4a *)
  | Eplus (e1, e2) -> begin
      match transition e1 with
      | Some e1' -> Some (Eplus (e1', e2))              (* 5.4b *)
      | None -> let* e2' = transition e2 in
                Some (Eplus (e1, e2'))                  (* 5.4c *)
    end
  | Etimes (Enum n1, Enum n2) ->
      Some (Enum (n1 * n2))
  | Etimes (e1, e2) -> begin
      match transition e1 with
      | Some e1' -> Some (Etimes (e1', e2))
      | None -> let* e2' = transition e2 in
                Some (Etimes (e1, e2'))
    end
  | Ecat (Estr n1, Estr n2) ->
      Some (Estr (n1 ^ n2))                             (* 5.4d *)
  | Ecat (e1, e2) -> begin
      match transition e1 with
      | Some e1' -> Some (Ecat (e1', e2))               (* 5.4e *)
      | None -> let* e2' = transition e2 in
                Some (Ecat (e1, e2'))                   (* 5.4f *)
    end
  | Elen (Estr s) ->
      Some (Enum (String.length s))
  | Elen e1 ->
      let* e1' = transition e1 in
      Some (Elen e1')
  | Elet (e1, x, e2) -> begin
      match transition e1 with
      | Some e1' -> Some (Elet (e1', x, e2))            (* 5.4g *)
      | None -> subst e1 x e2                           (* 5.4h *)
    end
  | Evar _ -> None

let rec normalize (e: exp) : exp =
  match transition e with
  | None -> e
  | Some e' -> normalize e'

(* Tests for normalize {{{ *)
let%test _ =
  let x = "x" in
  let zero, two = Enum 0, Enum 2 in
  let result = Enum 4 in
  result = normalize (Elet (Eplus (two, zero), x, Etimes (Evar x, two)))
(* }}} *)
