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
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

let ident = (lowercase | uppercase) identchar*
let number = ['0'-'9'] ['0'-'9' '_']*
let str = '"' identchar* '"'


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
  | '[' { LBRACK }
  | ']' { RBRACK }
  | "num" { NUM }
  | "str" { STR }
  | "plus" { PLUS }
  | "times" { TIMES }
  | "cat" { CAT }
  | "len" { LEN }
  | "let" { LET }
  | "." { DOT }
  | ";" { SEMICOLON }
  | "," { COMMA }
  (* lex identifiers last, so keywords are not lexed as identifiers *)
  | number as number { NUMBER (int_of_string number) }
  | ident as ident { IDENT ident }
  | str as str { STRING str }

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

