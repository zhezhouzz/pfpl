let parse filename =
    Parser.sexp_eof Lexer.next_token (Lexing.from_channel (open_in filename))
