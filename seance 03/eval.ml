open Types ;;
open Dict ;;
open Maybe ;;

exception Eval_exn of string ;;

let isval expr =
  match expr with
  | Function (_, _, _) -> true
  | _ -> false
;;

let eval_expr expr ctx =
  match expr with

  | Variable varname ->
    maybe_get (dict_get varname ctx) (Eval_exn ("unknown variable " ^ varname))

  | Function (_, _, _) ->
    expr

  | _ ->
    raise (Eval_exn "TODO")

  ;;

let eval expr ctx =
  match expr with

  | Variable _ ->
    let result = eval_expr expr ctx in
    print_string ((print_expression result) ^ "\n");
    ctx

  | Assignation (Variable varname, varexpr) ->
    let varresult = eval_expr varexpr ctx in
    dict_put varname varresult ctx

  | _ ->
    raise (Eval_exn "TODO")

  ;;