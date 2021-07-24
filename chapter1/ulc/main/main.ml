open Core

let regular_file =
  Command.Arg_type.create (fun filename ->
      match Sys.is_file filename with
      | `Yes -> filename
      | `No -> failwith "Not a regular file"
      | `Unknown -> failwith "Could not determine if this was a regular file")
let run =
  Command.basic
    ~summary:""
    Command.Let_syntax.(
      let%map_open inputfile = anon ("inputfile" %: regular_file)
      and strategy = anon ("strategy" %: string)
      in
      fun () -> Reduction.small_step_loop strategy (Parse.parse inputfile)
    )

let command =
  Command.group ~summary:""
    [ "run", run;
    ]

let () = Command.run command
;;
