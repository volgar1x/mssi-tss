open Types ;;
open Dict ;;
open Maybe ;;

exception Eval_exn of string ;;

let rec eval expr ctx =
  match expr with

  | Variable varname ->
    let varval = maybe_get (dict_get varname ctx)
                           (Eval_exn ("unknown variable " ^ varname)) in
    (varval, ctx)

  | Function (paramname, body, inner_ctx) ->
    let new_ctx = dict_merge ctx inner_ctx in
    let result = Function (paramname, body, new_ctx) in
    (result, ctx)

  | Application (Variable "pred", Application (Variable "succ", e)) -> eval e ctx
  | Application (Variable "succ", Application (Variable "pred", e)) -> eval e ctx

  | Application (Variable "succ", paramexpr) ->
    (match eval paramexpr ctx with
    | (Natural nat, new_ctx) -> (Natural (nat + 1), new_ctx)
    | _ -> raise (Eval_exn ("succ function is applicable only to naturals")))

  | Application (Variable "pred", paramexpr) ->
    (match eval paramexpr ctx with
    | (Natural nat, new_ctx) -> (Natural (nat - 1), new_ctx)
    | _ -> raise (Eval_exn ("pred function is applicable only to naturals")))

  | Application (fnexpr, paramexpr) ->
    let (paramresult, _) = eval paramexpr ctx in

    (match eval fnexpr ctx with
    | (Function (paramname, body, inner_ctx), _) ->
      let eval_ctx = dict_put paramname paramresult inner_ctx in
      let (eval_result, _) = eval body eval_ctx in
      (eval_result, ctx)
    | _ -> raise (Eval_exn ("Expression " ^ (print_expression fnexpr) ^ " is not applicable")))

  | Cond (cond, body, els) ->
    (match eval cond ctx with
    | (Boolean true, new_ctx) -> eval body new_ctx
    | (Boolean false, new_ctx) -> eval els new_ctx
    | (other, _) -> raise (Eval_exn ("cannot branch on expression " ^ (print_expression other))))

  | Bind (varname, varexpr, body) ->
    let (varresult, _) = eval varexpr ctx in
    let new_ctx = dict_put varname varresult ctx in
    eval body new_ctx

  | Natural _ -> (expr, ctx)
  | Boolean _ -> (expr, ctx)

  | Assignation (Variable varname, varexpr) ->
    let (varresult, _) = eval varexpr ctx in
    let new_ctx = dict_put varname varresult ctx in
    (varresult, new_ctx)

  ;;