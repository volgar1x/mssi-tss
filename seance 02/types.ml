type expression =
    | True
    | False
    | IfElse of expression * expression * expression
    | Zero
    | Succ of expression
    | Pred of expression
    | IsZero of expression
;;
