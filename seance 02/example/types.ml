type proposition =
  | True
  | False
  | Not of proposition
  | And of proposition * proposition
  | Or of proposition * proposition
  | Involve of proposition * proposition
  | Equiv of proposition * proposition
  | Var of string
;;