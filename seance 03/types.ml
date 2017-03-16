open Dict ;;

type expression = Variable of string
                | Function of string * expression * ((string * expression) list)
                | Application of expression * expression
                | Assignation of expression * expression
                ;;


let rec print_expression x =
  match x with
  | Variable var -> "Variable(" ^ var ^ ")"
  | Function (var, body, bound_ctx) -> "Function(" ^ var ^  ", " ^ (print_expression body) ^ ", " ^ (dict_str (dict_map_values print_expression bound_ctx)) ^ ")"
  | Application (left, right) -> "Application(" ^ (print_expression left) ^ ", " ^ (print_expression right) ^ ")"
  | Assignation (var, body) -> "Assignation(" ^ (print_expression var) ^  ", " ^ (print_expression body) ^ ")"
  ;;

let ctx_str ctx = dict_str (dict_map_values print_expression ctx) ;;
