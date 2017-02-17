type expression = Variable of string
                | Function of string * expression
                | Application of expression * expression
                | Assignation of expression * expression
                ;;

type 't maybe = Just of 't | Nothing ;;

let maybe_to_string = function
  | Just x -> "Just(" ^ x ^ ")"
  | Nothing -> "Nothing"
  ;;

let maybe_fmap fn x =
  match x with
  | Just y -> fn y
  | Nothing -> Nothing

let maybe_map fn x = maybe_fmap (fun x -> Just (fn x)) x

let maybe_get x err =
  match x with
  | Nothing -> raise err
  | Just y -> y
;;

type ('k, 'v) dictionary = 'k * 'v list ;;

let rec dict_get k1 d =
  match d with
  | [] -> Nothing
  | (k2, v) :: tl ->
    if String.equal k1 k2
    then Just v
    else dict_get k1 tl
  ;;

let dict_put k v d = (k, v) :: d

let dict_str xs =
  let rec aux xs acc =
    match xs with
    | [] -> acc ^ "}"
    | (k, v) :: tl -> aux tl (acc ^ ", " ^ k ^ ": " ^ v)
  in

  match xs with
  | [] -> "{}"
  | (k, v) :: tl -> aux tl ("{" ^ k ^ ": " ^ v)
  ;;

let dict_map_values fn xs =
  let rec aux xs acc =
    match xs with
    | [] -> List.rev acc
    | (k, v) :: tl -> aux tl ((k, fn v) :: acc)
  in

  aux xs []
;;
