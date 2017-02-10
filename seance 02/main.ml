let eval = function
    | True -> ValT
    | False -> ValF
    | Zero -> ValI 0

    | IfElse (True, e1, _) -> e1
    | IfElse (False, _, e2) -> e2
    | IfElse (cond, e1, e2) -> IfElse (eval cond) e1 e2

    | Succ e1 ->

    | Pred e1 ->

    | IsZero e1 ->

;;

let rec loop () =
  try
    print_string ">>> " ; 
    eval (Parser.line Lexer.lexer (
      (Lexing.from_string (read_line () ^"\n")))
    ) ;
    loop ()
  with End_of_file -> ()
;;

let _ = loop () ;;
