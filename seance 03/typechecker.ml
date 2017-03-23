open Types ;;
open Dict ;;
open Maybe ;;

exception Type_exn of string ;;

let etype_is_assignable_to fromT toT =
  match toT with
  | TParameter _ -> true
  | _ ->
    (match fromT with
    | TParameter _ -> true (* TODO *)
    | _ -> etype_equal fromT toT)
;;

let etype_assert_assignable_to fromT toT =
  if etype_is_assignable_to fromT toT
  then ()
  else raise (Type_exn ("cannot apply " ^ (etype_print fromT) ^ " to " ^ (etype_print toT)))
;;

let etype_apply t a = match t with
  | TParameter b -> (* just assume its a function for now *)
    TParameter (b ^ "2")

  | TFunction (b, c) ->
    etype_assert_assignable_to a b;
    c
    
  | _ -> raise (Type_exn ("type " ^ (etype_print t) ^ " is not applicable"))
;;

let rec type_of_expression gamma expr = match expr with
  | Natural _ -> TNatural
  | Boolean _ -> TBoolean
  | Variable "succ" -> TFunction (TNatural, TNatural)
  | Variable "pred" -> TFunction (TNatural, TNatural)
  | Variable varname ->
    (match dict_get varname gamma with
    | Nothing -> TParameter varname
    | Just t -> t)
  | Bind (varname, varexpr, body) ->
    let vartype = type_of_expression gamma varexpr in
    let new_gamma = dict_put varname vartype gamma in
    type_of_expression new_gamma body
  | Cond (cond, body, els) ->
    let condtype = type_of_expression gamma cond in

    (match condtype with
    | TBoolean | TParameter _ -> ()
    | _ -> raise (Type_exn ("only boolean are available in conditions but got: " ^ (print_expression cond))));

    let bodytype = type_of_expression gamma body in
    let elstype = type_of_expression gamma els in

    if etype_equal bodytype elstype
    then bodytype
    else raise (Type_exn "cond type mismatch")

  | Application (left, right) ->
    let leftT = type_of_expression gamma left in
    let rightT = type_of_expression gamma right in

    (match leftT with
    | TFunction _ | TParameter _ -> ()
    | _ -> raise (Type_exn ("cannot apply " ^ (etype_print rightT) ^ " to " ^ (etype_print leftT))));

    etype_apply leftT rightT
  | Function (param, body, _) ->
    let paramT = TParameter "T" in
    let new_gamma = dict_put param paramT gamma in
    let bodyT = type_of_expression new_gamma body in
    TFunction (paramT, bodyT)
  | _ -> raise (Type_exn ("unknown type mismatch: " ^ (print_expression expr)))
;;

let type_of_context ctx =
  let rec aux xs acc = match xs with
  | [] -> acc
  | (k, v) :: tl ->
    let v2 = type_of_expression acc v in
    aux tl ((k, v2) :: acc)
  in

  aux ctx []
;;