open Types ;;
open Dict ;;
open Maybe ;;

exception Type_exn of string ;;

type type_context = {
  next_param : string;
  context    : (string * etype) list;
};;

let empty_gamma = { next_param = "a"; context = [] };;

let next_param param = Char.escaped (Char.chr ((Char.code param.[0]) + 1)) ;;

let rec etype_is_assignable_to fromT toT =
  match toT with
  | TParam _ -> true
  | TForAll (_, e) -> etype_is_assignable_to fromT e
  | _ ->
    (match fromT with
    | TParam _ -> true (* TODO *)
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
  | TParam b -> (* just assume its a function for now *)
    TParam (b ^ "2")

  | TForAll (n, b) ->
    let tt = etype_replace b n a in
    etype_apply tt a

  | TFunction (b, c) ->
    etype_assert_assignable_to a b;
    c
    
  | _ -> raise (Type_exn ("type " ^ (etype_print t) ^ " is not applicable"))
;;

let multimap_merge xs =
  let multimap_add_all k vs xs =
    BatSet.PSet.fold
    (BatMultiPMap.add k)
    vs
    xs
  in

  let multimap_merge0 a b =
    BatMultiPMap.foldi
    multimap_add_all
    b a
  in

  List.fold_left multimap_merge0 BatMultiPMap.empty xs
;;

let multimap_etype_print xs =
  let print_key   out k = BatInnerIO.nwrite out k in
  let print_value out v = BatInnerIO.nwrite out (etype_print v) in
  let o = BatInnerIO.output_string () in
  BatMultiPMap.print ~first:"{" ~last:"}" ~sep:", " ~kvsep:": " print_key print_value o xs;
  BatInnerIO.close_out o
;;

let set_etype_print xs =
  let aux out t = BatInnerIO.nwrite out (etype_print t) in
  let o = BatInnerIO.output_string () in
  BatSet.PSet.print aux o xs;
  BatInnerIO.close_out o
;;

let rec unify expr expected rules gamma =
  match expr with
  | Natural _ ->
    rules (* TODO *)
  | Boolean _ ->
    rules (* TODO *)
  | Unit ->
    rules (* TODO *)

  | Function (param, body, _) ->
    let expected0 = match expected with
      | TFunction (_, x) -> x
      | TForAll (_, TFunction (_, x)) -> x
      | _ -> raise (Type_exn ("type " ^ (etype_print expected) ^ " needs to be applicable in (" ^ (print_expression expr) ^ ")"))
    in
    let rules1 = unify body expected0 rules gamma in
    rules1

  | Variable varname ->
    BatMultiPMap.add varname expected rules

  | Application (left, right) ->
    let param = next_param gamma.next_param in
    let new_gamma = { next_param = param; context = gamma.context } in
    let rightT = TParam param in
    let leftT = TForAll (param, TFunction (rightT, expected)) in

    let rules1 = unify left leftT rules new_gamma in
    let rules2 = unify right rightT rules new_gamma in
    multimap_merge [rules1; rules2]

  | Cond (condexpr, condbody, condelse) ->
    let rules1 = unify condexpr TBoolean rules gamma in
    let rules2 = unify condbody expected rules gamma in
    let rules3 = unify condelse expected rules gamma in
    
    multimap_merge [rules1; rules2; rules3]

  | Bind (bindname, bindexpr, bindbody) ->
    let rules1 = unify bindexpr expected rules gamma in
    let rules2 = unify bindbody expected rules gamma in

    multimap_merge [rules1; rules2]
;;

let rec type_of_expression gamma expr = match expr with
  | Variable "succ" -> TFunction (TNatural, TNatural)
  | Variable "pred" -> TFunction (TNatural, TNatural)
  | Natural _ -> TNatural
  | Boolean _ -> TBoolean
  | Unit -> TUnit

  | Variable varname ->
    (match dict_get varname gamma.context with
    | Nothing -> TParam gamma.next_param
    | Just t -> t)

  | Bind (varname, varexpr, body) ->
    let vartype = type_of_expression gamma varexpr in
    let new_gamma = dict_put varname vartype gamma.context in
    (match body with
    | Unit -> vartype
    | _ ->
      type_of_expression { next_param = gamma.next_param; context = new_gamma } body)

  | Cond (cond, body, els) ->
    let condtype = type_of_expression gamma cond in
    etype_assert_assignable_to TBoolean condtype;

    let bodytype = type_of_expression gamma body in
    let elstype = type_of_expression gamma els in

    (match etype_intersect bodytype elstype with
    | Just t -> t
    | Nothing -> raise (Type_exn ("cond type mismatch: " ^ (etype_print bodytype) ^ " & " ^ (etype_print elstype))))

  | Application (left, right) ->
    let leftT = type_of_expression gamma left in
    let rightT = type_of_expression gamma right in
    etype_apply leftT rightT

  | Function (paramname, body, _) ->
    let bodyT = type_of_expression gamma body in
    let rules = unify body bodyT BatMultiPMap.empty gamma in
    let paramrules = BatMultiPMap.find paramname rules in
    if (BatSet.PSet.cardinal paramrules) == 1 then
      let paramT = BatSet.PSet.at_rank_exn 0 paramrules in
      TFunction (paramT, bodyT)
    else if (BatSet.PSet.cardinal paramrules) > 1 then
      let etype_intersect0 left right =
        match etype_intersect left right with
        | Just t -> t
        | Nothing -> raise (Type_exn ("unification failed"))
      in
      let (paramrule, paramrules0) = BatSet.PSet.pop paramrules in
      let paramT = BatSet.PSet.fold etype_intersect0 paramrules0 paramrule in
      TFunction (paramT, bodyT)
    else
      let param = gamma.next_param in
      TForAll (param, TFunction(TParam param, bodyT))
;;

let type_of_context ctx =
  let rec aux xs acc = match xs with
  | [] -> { next_param = "a"; context = acc }
  | (k, v) :: tl ->
    let v2 = type_of_expression { next_param = "a"; context = acc } v in
    aux tl ((k, v2) :: acc)
  in

  aux ctx []
;;