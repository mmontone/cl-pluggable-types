type token =
  | VAR of (string)
  | LPAREN
  | RPAREN
  | IMP
  | EOL
  | FUN
  | APP

open Parsing;;
# 2 "Parser/parser.mly"
open Ast
# 14 "Parser/parser.ml"
let yytransl_const = [|
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* IMP *);
  261 (* EOL *);
  262 (* FUN *);
  263 (* APP *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\004\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\000\000\006\000\000\000\000\000\
\000\000\001\000\005\000\004\000\000\000\000\000"

let yydgoto = "\002\000\
\006\000\011\000"

let yysindex = "\003\000\
\013\255\000\000\000\000\013\255\006\255\000\000\000\255\007\255\
\012\255\000\000\000\000\000\000\013\255\020\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\015\255"

let yygindex = "\000\000\
\000\000\255\255"

let yytablesize = 22
let yytable = "\007\000\
\003\000\004\000\008\000\001\000\010\000\005\000\009\000\003\000\
\004\000\012\000\000\000\014\000\005\000\003\000\004\000\013\000\
\000\000\003\000\005\000\003\000\003\000\004\000"

let yycheck = "\001\000\
\001\001\002\001\004\000\001\000\005\001\006\001\001\001\001\001\
\002\001\003\001\255\255\013\000\006\001\001\001\002\001\004\001\
\255\255\003\001\006\001\005\001\001\001\002\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  IMP\000\
  EOL\000\
  FUN\000\
  APP\000\
  "

let yynames_block = "\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 22 "Parser/parser.mly"
                       ( _1 )
# 83 "Parser/parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 26 "Parser/parser.mly"
                        ( Var _1 )
# 90 "Parser/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 27 "Parser/parser.mly"
                               ( Fun (_2, _4) )
# 98 "Parser/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 28 "Parser/parser.mly"
                        ( _2 )
# 105 "Parser/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 29 "Parser/parser.mly"
                        ( App (_1, _2) )
# 113 "Parser/parser.ml"
               : 'expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
