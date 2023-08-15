(*****************************
 * Main read-eval-print loop *
 *****************************)

let parse (s : string) : Ast.expr =
  Parser.main Lexer.token (Lexing.from_string s)
  
let i = "fun x -> x"
let s = "fun x -> fun y -> fun z -> x z (y z)"
let k = "fun x -> fun y -> x"
let o = "fun f -> fun g -> fun x -> f (g x)"
let y = "fun f -> (fun x -> f x x) (fun y -> f y y)"
  
let rec repl () =
  print_string "? ";
  let input = read_line() in
  if input = "" then () else
  try
    let e = parse input in
    let t = Infer.infer e in
    Printf.printf "%s : %s\n" (Ast.to_string e) (Ast.type_to_string t);
    repl ()
  with Failure msg -> print_endline msg; repl ()
  | _ -> print_endline "Error"; repl ()
  
let _ = repl()
