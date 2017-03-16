open Types ;;
open Dict ;;
open Eval ;;

let debug_expression x =
  print_string ((print_expression x) ^ "\n")
;;

let rec loop ctx =
  try
    print_string ">>> ";

    let expr = Parser.line Lexer.lexer ((Lexing.from_string (read_line () ^"\n"))) in

    debug_expression expr;
    let new_ctx = eval expr ctx in

    print_string ((dict_str (dict_map_values print_expression new_ctx)) ^ "\n");

    loop new_ctx

  with
  | End_of_file -> ()
  | Eval_exn msg ->
    print_string ("Error: " ^ msg ^ "\n");
    loop ctx
;;

let _ = loop [] ;;
