open Maybe ;;

type ('k, 'v) dictionary = ('k * 'v) list ;;

let rec dict_get k1 d =
  match d with
  | [] -> Nothing
  | (k2, v) :: tl ->
    if String.equal k1 k2
    then Just v
    else dict_get k1 tl
  ;;

let dict_put k1 v1 d =
  let rec aux xs acc =
    match xs with
    | [] -> (k1, v1) :: acc
    | (k2, v2) :: tl ->
      if String.equal k1 k2
      then aux tl acc
      else aux tl ((k2, v2) :: acc)
  in

  aux d []
  ;;

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

let dict_keys xs =
  let rec aux xs acc =
    match xs with
    | [] -> acc
    | (k, _) :: tl -> aux tl (k::acc)
  in

  aux xs []
;;

let dict_merge a b =
  let rec aux xs acc =
    match xs with
    | [] -> acc
    | (k, v) :: tl -> aux tl (dict_put k v acc)
  in
  aux b a
  ;;

print_string ("dict_merge: " ^ (dict_str (dict_merge (dict_put "a" "hello" []) (dict_put "a" "world" []))) ^ "\n") ;;
