%{
    open Ast
    let make_fun args body =
      let rec aux args =
        match args with
        | [] -> body
        | (info, hd) :: tl -> info, Fun (hd, aux tl)
      in
      aux args
    let make_app es =
      let rec aux es =
        match es with
        | [] -> failwith "never happen"
        | [e] -> e
        | (info, h) :: tl -> info, App (aux tl, (info, h))
      in
      aux (List.rev es)
%}
(* tokens *)
%token EOF LPAR RPAR FUN ARROW ID
%token <string> ATOM

(* start symbol *)
%start <Ast.t> sexp_eof

%%

sexp_eof:
  | e=sexp; EOF { e }
  ;
args:
  | a=ATOM {[$startpos, a]}
  | a=ATOM b=args {($startpos, a) :: b}
  ;
sexp_list:
  | x=sexp {[x]}
  | x=sexp_list y=sexp {x @ [y]}
sexp:
  | FUN x=args ARROW b=sexp {make_fun x b}
  | LPAR a=sexp_list RPAR {make_app a }
  | ID {$startpos, Ast.Fun ("x", ($startpos, Ast.Var "x"))}
  | a=ATOM {$startpos, Ast.Var a }
  ;

%%
