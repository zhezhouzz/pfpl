open Past

let remove_info abtt =
  let rec aux (_, abt) =
    match abt with
    | Aleaf x -> Abt.Aleaf x
    | Anode ((_, op), bindings) ->
      Abt.Anode (op, List.map (fun (xs, abtt) -> xs, aux abtt) bindings)
  in
  aux abtt

let parse filename =
  remove_info @@ Parser.abtt_eof Lexer.next_token (Lexing.from_channel (open_in filename))
