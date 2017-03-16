let rec impairs_0 xs =
  match xs with
  | [] -> []
  | hd :: tl ->
      if (hd mod 2) == 0
      then (impairs_0 tl)
      else hd :: (impairs_0 tl)
;;

let impairs_0_test_1 = impairs_0 [1; 2; 3; 4; 5] ;;

let impairs xs =
  let rec aux xs acc =
    match xs with
    | [] -> List.rev acc
    | hd :: tl ->
        if (hd mod 2) == 0
        then (aux tl acc)
        else (aux tl (hd :: acc))
  in

  aux xs []
;;

let impairs_test_1 = impairs [1; 2; 3; 4; 5] ;;

let merge xs =
  let rec aux xs acc =
    match xs with
    | [] -> acc
    | hd :: tl -> aux tl acc
  in

  aux xs []
;;

let merge_test_1 = merge [[1; 2]; [3]; [4; 5; 6; 7]; [8; 9; 10]] ;;
