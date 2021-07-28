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
