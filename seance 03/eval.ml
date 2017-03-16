open Types ;;
open Dict ;;
open Maybe ;;

exception Eval_exn of string ;;

let isval expr =
  match expr with
  | Function (_, _, _) -> true
  | _ -> false
;;

let rec eval_expr expr ctx =
  match expr with

  | Variable varname ->
    maybe_get (dict_get varname ctx) (Eval_exn ("unknown variable " ^ varname))

  | Function (paramname, body, inner_ctx) ->
    let new_ctx = dict_merge ctx inner_ctx in
    Function (paramname, body, new_ctx)

  | Application (fnexpr, paramexpr) ->
    let paramresult = eval_expr paramexpr ctx in

    match eval_expr fnexpr ctx with
    | Function (paramname, body, inner_ctx) ->
      let eval_ctx = dict_put paramname paramresult inner_ctx in
      eval_expr body eval_ctx
    | _ -> raise (Eval_exn "")

  | _ ->
    raise (Eval_exn "TODO")

  ;;

let eval expr ctx =
  match expr with

  | Assignation (Variable varname, varexpr) ->
    let varresult = eval_expr varexpr ctx in
    dict_put varname varresult ctx

  | _ ->
    let result = eval_expr expr ctx in
    print_string ((print_expression result) ^ "\n");
    ctx

  ;;