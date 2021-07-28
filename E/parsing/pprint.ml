open Abt
open Format

let rec pprint_op ff abt =
  let rec aux abt =
    match op with
    | Onum i -> pp_print_int ff i
    | Ostr s -> pp_print_string ff s
    | Oplus -> pp_print_string ff "plus"
    | Otimes -> pp_print_string ff "times"
    | Ocat -> pp_print_string ff "times"
    | Olen                (*      len(e)        *)
    | Olet                (*      let(e1; x.e2) *)


let rec pprint ff abt =
  let rec aux abt =
    match abt with
    | Aleaf x -> pp_print_string ff x
    | Anode (op, bindings) ->
      
