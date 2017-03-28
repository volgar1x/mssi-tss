open Types ;;
open Eval ;;

let load_stdlib () =
  let ic = open_in "./stdlib.lambda" in

  let rec aux acc =
    try
      let line = input_line ic in
      if (String.length (String.trim line)) > 0 then
        let expr = Parser.line Lexer.lexer (Lexing.from_string (line ^ "\n")) in
        let (_, new_ctx) = eval expr acc in
        aux new_ctx
      else
        aux acc
    with End_of_file ->
      close_in ic;
      acc
  in

  aux []
  ;;
