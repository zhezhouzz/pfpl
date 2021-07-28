%{
    (* open Past *)
%}
(* tokens *)
%token EOF LPAR RPAR LBRACK RBRACK
(* keywords *)
%token NUM STR PLUS TIMES CAT LEN LET DOT SEMICOLON COMMA
%token <string> STRING IDENT
%token <int> NUMBER

(* start symbol *)
%start <Past.abtt> abtt_eof

%%

abtt_eof:
  | e=abtt; EOF { e }
  ;
abtt:
  | o=opt LPAR a=bindings RPAR {$startpos, Past.Anode (o, a)}
  | a=IDENT {$startpos, Past.Aleaf a }
  ;
bindings:
  | b=binding SEMICOLON s=bindings {b :: s}
  | b=binding {[b]}
  ;
binding:
  | a=abtt {([], a)}
  | v=vars DOT a=abtt {(v, a)}
  ;
vars:
  | x=IDENT COMMA v=vars {x :: v}
  | x=IDENT {[x]}
  ;
opt:
  | NUM LBRACK a=NUMBER RBRACK {$startpos, Abt.Onum a}
  | STR LBRACK a=STRING RBRACK {$startpos, Abt.Ostr a}
  | PLUS {$startpos, Abt.Oplus}
  | TIMES {$startpos, Abt.Otimes}
  | CAT {$startpos, Abt.Ocat}
  | LEN {$startpos, Abt.Olen}
  | LET {$startpos, Abt.Olet}
  ;
%%
