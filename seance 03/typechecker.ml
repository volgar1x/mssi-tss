open Types ;;
open Dict ;;
open Maybe ;;

exception Type_exn of string ;;

let rec etype_is_assignable_to fromT toT =
  match toT with
  | TName _ -> true
  | TForAll (_, e) -> etype_is_assignable_to fromT e
  | _ ->
    (match fromT with
    | TName _ -> true (* TODO *)
    | TForAll (_, e) -> etype_is_assignable_to e toT
    | _ -> etype_equal fromT toT)
;;

let etype_assert_assignable_to fromT toT =
  if etype_is_assignable_to fromT toT
  then ()
  else raise (Type_exn ("cannot assign " ^ (etype_print fromT) ^ " to " ^ (etype_print toT)))
;;

let rec etype_apply t a =
  match t with
  | TName b -> (* just assume its a function for now *)
    TName (b ^ "2")

  | TForAll (n, b) ->
    let tt = etype_replace b n a in
    etype_apply tt a

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
    | Nothing -> TName varname
    | Just t -> t)
  | Bind (varname, varexpr, body) ->
    let vartype = type_of_expression gamma varexpr in
    let new_gamma = dict_put varname vartype gamma in
    type_of_expression new_gamma body
  | Cond (cond, body, els) ->
    let condtype = type_of_expression gamma cond in
    etype_assert_assignable_to TBoolean condtype;

    let bodytype = type_of_expression gamma body in
    let elstype = type_of_expression gamma els in

    if etype_equal bodytype elstype
    then bodytype
    else raise (Type_exn "cond type mismatch")

  | Application (left, right) ->
    let leftT = type_of_expression gamma left in
    let rightT = type_of_expression gamma right in
    etype_apply leftT rightT
  | Function (param, body, _) ->
    let paramT = TName "T" in
    let new_gamma = dict_put param paramT gamma in
    let bodyT = type_of_expression new_gamma body in
    TForAll ("T", TFunction (paramT, bodyT))
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