open Types ;;
open Dict ;;
open Maybe ;;
open Eval ;;
open Stdlib ;;
open Typechecker ;;

let rec loop ctx =
  try
    print_string ">>> ";

    let expr = Parser.line Lexer.lexer (Lexing.from_string (read_line () ^ "\n")) in    

    try
      debug_gamma (type_of_context ctx);
      print_string ("=> " ^ (etype_print (type_of_expression (type_of_context ctx) expr)) ^ "\n");
      (* debug_expression expr; *)
      let new_ctx = eval expr ctx in
      (* debug_ctx new_ctx; *)

      loop new_ctx
    with Type_exn msg ->
      print_string ("Type mismatch: " ^ msg ^ "\n");
      loop ctx

  with
  | End_of_file -> ()
  | Eval_exn msg ->
    print_string ("Error: " ^ msg ^ "\n");
    loop ctx
;;

let stdlib = load_stdlib () ;;
let _ = loop stdlib ;;
