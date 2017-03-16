open Dict ;;

type expression = Variable of string
                | Function of string * expression * ((string * expression) list)
                | Application of expression * expression
                | Assignation of expression * expression
                ;;