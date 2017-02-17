open Types ;;

let rec print_expression x =
  match x with
  | Variable var -> "Variable(" ^ var ^ ")"
  | Function (var, body) -> "Function(" ^ var ^  ", " ^ (print_expression body) ^ ")"
  | Application (left, right) -> "Application(" ^ (print_expression left) ^ ", " ^ (print_expression right) ^ ")"
  | Assignation (var, body) -> "Assignation(" ^ (print_expression var) ^  ", " ^ (print_expression body) ^ ")"
  ;;

let evaldbg x =
  print_string ((print_expression x) ^ "\n");
  []
;;

exception Eval_exn of string ;;

let rec eval_application left right ctx =
  let rec aux pname pval body =
    match body with
    | Variable var ->
      if String.equal var pname
      then pval
      else maybe_get (dict_get var ctx) (Eval_exn ("undefined variable " ^ var))
    | Function (pname2, body2) ->
      raise (Eval_exn "todo")
    | x -> x
  in

  match left with
  | Function (param, body) -> aux param right body

  | Application (left2, right2) -> raise (Eval_exn "todo")

  | Variable var ->
    (match dict_get var ctx with
    | Nothing -> raise (Eval_exn ("undefined function " ^ var))
    | Just var_val -> eval_application var_val right ctx)

  | _ -> raise (Eval_exn "invalid syntax")
;;

let eval x ctx =
  match x with
  | Variable var ->
    (match dict_get var ctx with
    | Nothing -> raise (Eval_exn ("undefined variable " ^ var))
    | Just var_val -> print_string ((print_expression var_val) ^ "\n"));
    
    ctx

  | Function (var, body) -> ctx

  | Application (left, right) ->
    let result = eval_application left right ctx in
    print_string ((print_expression result) ^ "\n");
    ctx

  | Assignation (Variable var, body) -> (dict_put var body ctx)

  | _ ->
    raise (Eval_exn "")
;;

let rec loop ctx =
  try
    print_string ">>> ";

    let expr = Parser.line Lexer.lexer ((Lexing.from_string (read_line () ^"\n"))) in

    (* let new_ctx = eval expr ctx in *)
    let new_ctx = evaldbg expr in

    print_string ((dict_str (dict_map_values print_expression new_ctx)) ^ "\n");

    loop new_ctx

  with
  | End_of_file -> ()
  | Eval_exn msg ->
    print_string ("Error: " ^ msg ^ "\n");
    loop ctx
;;

let _ = loop [] ;;
