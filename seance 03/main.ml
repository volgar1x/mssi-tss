open Types ;;
open Dict ;;
open Maybe ;;
open Eval ;;
open Stdlib ;;
open Typechecker ;;
open Lexing ;;

let rec loop gamma ctx =
  let parse buf =
    try
      Parser.line Lexer.lexer buf
    with
    | Parsing.Parse_error ->
      raise (Eval_exn ("Parse Error on column " ^ (string_of_int buf.lex_curr_pos) ^ " `" ^ (Lexing.lexeme buf) ^ "'"))
    | Failure reason ->
      raise (Eval_exn ("Parse Error on column " ^ (string_of_int buf.lex_curr_pos) ^ " `" ^ (Lexing.lexeme buf) ^ "' because: " ^ reason))
  in

  let eval_print line =
    let expr = parse (Lexing.from_string line) in
    (* debug_expression expr; *)

    let exprT = type_of_expression gamma expr in  
    print_endline ("=> " ^ (etype_print exprT));
    
    let (result, new_ctx) = eval expr ctx in
    print_endline (print_expression result);
    new_ctx
  in

  try
    print_string ">>> ";
    let line = read_line () in
    if (String.length (String.trim line)) > 0 then
      let new_ctx = eval_print line in
      (* debug_ctx new_ctx; *)
      loop gamma new_ctx
    else
      loop gamma ctx

  with
  | End_of_file -> ()
  | Eval_exn msg ->
    print_string ("Error: " ^ msg ^ "\n");
    loop gamma ctx
  | Type_exn msg ->
    print_string ("Type mismatch: " ^ msg ^ "\n");
    loop gamma ctx
;;

let (stdlib_gamma, stdlib) = load_stdlib () in
debug_gamma stdlib_gamma.context;
loop stdlib_gamma stdlib
;;