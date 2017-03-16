open Types ;;
open Dict ;;
open Maybe ;;

let rec print_expression x =
  match x with
  | Variable var -> "Variable(" ^ var ^ ")"
  | Function (var, body, bound_ctx) -> "Function(" ^ var ^  ", " ^ (print_expression body) ^ ", " ^ (dict_str (dict_map_values print_expression bound_ctx)) ^ ")"
  | Application (left, right) -> "Application(" ^ (print_expression left) ^ ", " ^ (print_expression right) ^ ")"
  | Assignation (var, body) -> "Assignation(" ^ (print_expression var) ^  ", " ^ (print_expression body) ^ ")"
  ;;

let evaldbg x =
  print_string ((print_expression x) ^ "\n");
  []
;;

let ctx_str ctx = dict_str (dict_map_values print_expression ctx) ;;

exception Eval_exn of string ;;

let rec eval_application left right ctx =
  print_string ("applying " ^ (print_expression right) ^ " to " ^ (print_expression left) ^ " in " ^ (ctx_str ctx) ^ "\n");

  let eval_expr e ctx =
    print_string ("evaluating expression " ^ (print_expression e) ^ " in " ^ (ctx_str ctx) ^ "\n");
    match e with
    | Variable vname -> maybe_get (dict_get vname ctx) (Eval_exn ("undefined variable " ^ vname))
    | Application (left, right) -> eval_application left right ctx
    | x -> x
  in

  let rec aux pname pval body ctx =
    print_string ("applying " ^ pname ^ "=" ^ (print_expression pval) ^ " to " ^ (print_expression body) ^ " in " ^ (ctx_str ctx) ^ "\n");
    match body with
    | Variable var ->
      eval_expr pval ctx
    | Function (pname2, body2, bound_ctx) ->
      eval_expr body2 (dict_put pname pval bound_ctx)
    | x -> x
  in

  match left with
  | Function (param, body, bound_ctx) ->
    let param_value = eval_expr right ctx in
    let eval_ctx = dict_merge ctx bound_ctx in
    aux param param_value body eval_ctx

  | Application (left2, right2) ->
    let result = eval_application left2 right2 ctx in
    eval_application result right ctx

  | Variable var ->
    let var_val = (maybe_get (dict_get var ctx) (Eval_exn ("undefined function " ^ var))) in
    eval_application var_val right ctx

  | _ -> raise (Eval_exn "invalid syntax")
;;

let eval x ctx =
  print_string ("topeval " ^ (print_expression x) ^ " in " ^ (ctx_str ctx) ^ "\n");
  match x with
  | Variable var ->
    print_string ((print_expression (maybe_get (dict_get var ctx) (Eval_exn ("undefined variable " ^ var)))) ^ "\n");
    
    ctx

  | Function (var, body, bound_ctx) ->
    ctx

  | Application (left, right) ->
    let result = eval_application left right ctx in
    print_string ((print_expression result) ^ "\n");
    ctx

  | Assignation (Variable var, body) ->
    dict_put var body ctx

  | _ ->
    raise (Eval_exn "")
;;

let rec loop ctx =
  try
    print_string ">>> ";

    let expr = Parser.line Lexer.lexer ((Lexing.from_string (read_line () ^"\n"))) in

    let new_ctx = eval expr ctx in
    (* let new_ctx = evaldbg expr in *)

    print_string ((dict_str (dict_map_values print_expression new_ctx)) ^ "\n");

    loop new_ctx

  with
  | End_of_file -> ()
  | Eval_exn msg ->
    print_string ("Error: " ^ msg ^ "\n");
    loop ctx
;;

print_string ("dict_merge: " ^ (dict_str (dict_merge (dict_put "a" "hello" []) (dict_put "a" "world" []))) ^ "\n") ;;

let _ = loop [] ;;
