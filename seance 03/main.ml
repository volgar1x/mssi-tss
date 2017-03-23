open Types ;;
open Dict ;;
open Eval ;;
open Stdlib ;;

let rec loop ctx =
  try
    print_string ">>> ";

    let expr = Parser.line Lexer.lexer (Lexing.from_string (read_line () ^ "\n")) in

    (* debug_expression expr; *)
    let new_ctx = eval expr ctx in
    (* debug_ctx new_ctx; *)

    loop new_ctx

  with
  | End_of_file -> ()
  | Eval_exn msg ->
    print_string ("Error: " ^ msg ^ "\n");
    loop ctx
;;

let _ = loop (load_stdlib ()) ;;
