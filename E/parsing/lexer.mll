{
open Parser

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)

open Lexing
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* regular expressions *)
let whitespace = ' ' | '\t'
let newline = "\r\n" | '\r' | '\n'
let ident = ['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule next_token = parse
  | eof { EOF }
  | whitespace+
    { next_token lexbuf }
  | newline
      { next_line lexbuf; next_token lexbuf }
  | "(*"
    { comment 0 lexbuf; next_token lexbuf }

  (* YOUR TOKENS HERE... *)
  | '(' { LPAR }
  | ')' { RPAR }
  | "id" { ID }
  | "fun" { FUN }
  | "->" { ARROW }
  (* lex identifiers last, so keywords are not lexed as identifiers *)
  | ident as atom { ATOM atom }

  (* no match? raise exception *)
  | _ as c { illegal c }


(* allow nested comments, like OCaml *)
and comment nesting = parse
  | "(*"
    { comment (nesting+1) lexbuf }
  | "*)"
    { if nesting > 0 then comment (nesting - 1) lexbuf }
  | eof
    { failwith "[lexer] unterminated comment at EOF" }
  | _
    { comment nesting lexbuf }

