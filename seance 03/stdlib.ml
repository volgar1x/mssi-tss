open Types ;;
open Eval ;;

let load_stdlib () =
  let ic = open_in "./stdlib.lambda" in

  let rec aux acc =
    try
      let line = input_line ic in
      let expr = Parser.line Lexer.lexer (Lexing.from_string (line ^ "\n")) in
      aux (eval expr acc)
    with End_of_file ->
      close_in ic;
      acc
  in

  aux []
  ;;
