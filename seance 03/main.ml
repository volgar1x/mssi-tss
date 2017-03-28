open Types ;;
open Dict ;;
open Maybe ;;
open Eval ;;
open Stdlib ;;
open Typechecker ;;

let rec loop ctx =
  let eval_print line =
    let expr = Parser.line Lexer.lexer (Lexing.from_string (line ^ "\n")) in

    (* debug_expression expr; *)
    let exprT = type_of_expression (type_of_context ctx) expr in  
    print_endline ("=> " ^ (etype_print exprT));
    
    eval expr ctx
  in

  try
    print_string ">>> ";
    let line = read_line () in
    if (String.length (String.trim line)) > 0 then
      let new_ctx = eval_print line in
      (* debug_ctx new_ctx; *)
      loop new_ctx
    else
      loop ctx

  with
  | End_of_file -> ()
  | Eval_exn msg ->
    print_string ("Error: " ^ msg ^ "\n");
    loop ctx
  | Type_exn msg ->
    print_string ("Type mismatch: " ^ msg ^ "\n");
    loop ctx
;;

let stdlib = load_stdlib () ;;
debug_gamma (type_of_context stdlib) ;;
let _ = loop stdlib ;;
