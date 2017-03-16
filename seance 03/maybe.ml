type 't maybe = Just of 't | Nothing ;;

let maybe_to_string = function
  | Just x -> "Just(" ^ x ^ ")"
  | Nothing -> "Nothing"
  ;;

let maybe_fmap fn x =
  match x with
  | Just y -> fn y
  | Nothing -> Nothing
  ;;

let maybe_map fn x = maybe_fmap (fun x -> Just (fn x)) x ;;

let maybe_get x err =
  match x with
  | Nothing -> raise err
  | Just y -> y
;;