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
    (match dict_get var ctx with
    | Nothing -> raise (Eval_exn ("undefined variable " ^ var))
    | Just var_val -> print_string ((print_expression var_val) ^ "\n"));
    
    ctx

  | Function (var, body) -> ctx

  | Application (left, right) -> ctx

  | Assignation (Variable var, body) -> (dict_put var body ctx)

  | _ ->
    raise (Eval_exn "")
;;

let rec loop ctx =
  try
    print_string ">>> ";

    let expr = Parser.line Lexer.lexer ((Lexing.from_string (read_line () ^"\n"))) in

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
