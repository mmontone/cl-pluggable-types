type token =
  | VAR of (string)
  | LPAREN
  | RPAREN
  | IMP
  | EOL
  | FUN
  | APP

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
