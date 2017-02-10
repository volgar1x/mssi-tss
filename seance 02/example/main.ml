let rec loop () =
  try
    print_string ">>> " ; 
    Eval.examine (Parser.line Lexer.lexer (
      (Lexing.from_string (read_line () ^"\n")))
    ) ;
    loop ()
  with End_of_file -> ()
;;

let _ = loop () ;;
