open Core

let regular_file =
  Command.Arg_type.create (fun filename ->
      match Sys.is_file filename with
      | `Yes -> filename
      | `No -> failwith "Not a regular file"
      | `Unknown -> failwith "Could not determine if this was a regular file")
let read =
  Command.basic
    ~summary:""
    Command.Let_syntax.(
      let%map_open inputfile = anon ("inputfile" %: regular_file)
      in
      fun () ->
        let abt : Abt.abt = Parsing.Parse.parse inputfile in
        let () = Parsing.Pprint.print_abt abt in
        ()
    )

let command =
  Command.group ~summary:""
    [ "read", read;
    ]

let () = Command.run command
;;
