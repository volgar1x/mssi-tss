{
  open Parser ;;
}
rule lexer = parse

  | [' ' '\t']          {lexer lexbuf}
  | '\n'                {Leol}

  | "vrai"              {Ltrue}
  | "faux"              {Lfalse}
  | "if"                {Lif}
  | "then"              {Lthen}
  | "else"              {Lelse}
  | "0"                 {Lzero}
  | "succ"              {Lsucc}
  | "pred"              {Lpred}
  | "iszero"            {Liszero}
