open Types ;;
open Dict ;;
open Maybe ;;

exception Eval_exn of string ;;

let isval expr =
  match expr with
  | Function (_, _, _) -> true
  | _ -> false
;;

let eval expr ctx =
  match expr with

  | Variable varname ->
    let result = maybe_get (dict_get varname ctx) (Eval_exn ("unknown variable " ^ varname)) in
    print_string ((print_expression result) ^ "\n");
    ctx

  | _ ->
    raise (Eval_exn "invalid expression")

  ;;