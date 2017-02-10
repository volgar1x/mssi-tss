open Types ;;

let rec print_expression x =
  match x with
  | Variable var -> "Variable(" ^ var ^ ")"
  | Function (var, body) -> "Function(" ^ (print_expression var) ^  ", " ^ (print_expression body) ^ ")"
  | Application (left, right) -> "Application(" ^ (print_expression left) ^ ", " ^ (print_expression right) ^ ")"
  | Assignation (var, body) -> "Assignation(" ^ (print_expression var) ^  ", " ^ (print_expression body) ^ ")"
  ;;

exception Eval_exn of string ;;

let eval x ctx =
  match x with
  | Variable var ->
    (maybe_to_string (maybe_map print_expression (dict_get var ctx))
    , ctx)
  | Function (var, body) ->
    ("0", ctx)
  | Application (left, right) ->
    ("0", ctx)
  | Assignation (Variable var, body) ->
    (var, dict_put var body ctx)
  | _ ->
    raise (Eval_exn "")

let rec loop ctx =
  try
    print_string ">>> ";

    let expr = Parser.line Lexer.lexer ((Lexing.from_string (read_line () ^"\n"))) in

    match (eval expr ctx) with
    | (result, new_ctx) ->
      print_string result;
      print_string "\n";
      print_string (dict_str (dict_map_values print_expression new_ctx)) ^ "\n";
      loop new_ctx

  with
  | End_of_file -> ()
  | Eval_exn msg ->
    print_string ("Error: " ^ msg ^ "\n");
    ()
;;

let _ = loop [] ;;
