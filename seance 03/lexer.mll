{
  open Parser ;;
}

rule lexer = parse

  | [' ' '\t']          {lexer lexbuf}
  | '\n'                {Leol}

  | "("                 {Loparen}
  | ")"                 {Lcparen}
  | "lambda"            {Llambda}
  | "."                 {Ldot}
  | "let"               {Llet}
  | "="                 {Leq}
  | ['a'-'z' 'A'-'Z']+  {Lident (Lexing.lexeme lexbuf)}
