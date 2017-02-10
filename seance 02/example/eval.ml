open Types ;;

let rec proposition2str x =
    match x with
    | True -> "true"
    | False -> "false"
    | Not p -> "not" ^ (proposition2str p)
    | And (p1, p2) -> "and" ^ (proposition2str p1)
    | Or (p1, p2) -> "or" ^ (proposition2str p1)
    | Involve (p1, p2) -> "involve" ^ (proposition2str p1)
    | Equiv (p1, p2) -> "equiv" ^ (proposition2str p1)
    | Var v -> "var" ^ v
;;

let examine p = print_string ((proposition2str p) ^ "\n") ;;
