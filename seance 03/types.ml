open Dict ;;

type expression = Variable of string
                | Function of string * expression * ((string * expression) list)
                | Application of expression * expression
                | Assignation of expression * expression
                | Natural of int
                | Boolean of bool
                | Cond of expression * expression * expression
                | Bind of string * expression * expression
                ;;


let rec print_expression x =
  match x with
  | Variable var -> "Variable(" ^ var ^ ")"
  | Function (var, body, bound_ctx) -> "Function(" ^ var ^  ", " ^ (print_expression body) ^ ", " ^ (dict_str (dict_map_values print_expression bound_ctx)) ^ ")"
  | Application (left, right) -> "Application(" ^ (print_expression left) ^ ", " ^ (print_expression right) ^ ")"
  | Assignation (var, body) -> "Assignation(" ^ (print_expression var) ^  ", " ^ (print_expression body) ^ ")"
  | Natural nat -> "Natural(" ^ (string_of_int nat) ^ ")"
  | Boolean b -> "Boolean(" ^ (if b then "true" else "false") ^ ")"
  | Cond (cond, body, els) -> "Cond(" ^ (print_expression cond) ^ "," ^ (print_expression body) ^ "," ^ (print_expression els) ^ ")"
  | Bind (varname, varexpr, body) -> "Bind(" ^ varname ^ "," ^ (print_expression varexpr) ^ "," ^ (print_expression body) ^ ")"
  ;;

let ctx_str ctx = dict_str (dict_map_values print_expression ctx) ;;


let debug_expression x =
  print_string ((print_expression x) ^ "\n")
;;

let debug_ctx ctx =
  print_string ((ctx_str ctx) ^ "\n")
;;
