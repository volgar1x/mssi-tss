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


type etype = TBoolean
           | TNatural
           | TFunction of etype * etype
           | TParameter of string
           ;;

let print_ctx_keys xs =
  "[" ^
  (String.concat ", " (dict_keys xs)) ^
  "]"
;;

let rec print_expression x =
  match x with
  | Variable var -> var
  | Function (var, body, bound_ctx) -> "λ" ^ var ^  ". " ^ (print_expression body)
  | Application (left, right) -> "(" ^ (print_expression left) ^ " " ^ (print_expression right) ^ ")"
  | Assignation (var, body) -> "global " ^ (print_expression var) ^  " = " ^ (print_expression body)
  | Natural nat -> (string_of_int nat)
  | Boolean b -> (if b then "true" else "false")
  | Cond (cond, body, els) -> "if (" ^ (print_expression cond) ^ ") then " ^ (print_expression body) ^ " else " ^ (print_expression els)
  | Bind (varname, varexpr, body) -> "let " ^ varname ^ " = " ^ (print_expression varexpr) ^ " in " ^ (print_expression body)
  ;;

let ctx_str ctx = dict_str (dict_map_values print_expression ctx) ;;

let rec etype_print = function
  | TBoolean -> "TBoolean"
  | TNatural -> "TNatural"
  | TFunction (a, b) ->
    (match a with
    | TFunction (_, _) -> "(" ^ (etype_print a) ^ ") -> " ^ (etype_print b)
    | _ -> (etype_print a) ^ " -> " ^ (etype_print b))
  | TParameter t -> "[" ^ t ^ "]"
;;

let rec etype_equal left right = match (left, right) with
  | (TBoolean, TBoolean) -> true
  | (TNatural, TNatural) -> true
  | (TParameter a, TParameter b) -> String.equal a b
  | (TFunction (a, b), TFunction (c, d)) -> (etype_equal a c) && (etype_equal b d)
  | _ -> false
;;

let debug_expression x =
  print_string ((print_expression x) ^ "\n")
;;

let debug_ctx ctx =
  print_string ((ctx_str ctx) ^ "\n")
;;

let debug_gamma g =
  print_string ((dict_str (dict_map_values etype_print g)) ^ "\n")
;;
