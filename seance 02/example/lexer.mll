{
  open Parser ;;
}
rule lexer = parse                       (* nom de la fonction construite par ocamllex pour *)
                                         (* détecter des lexèmes dans un flux de caractères *)

  | [' ' '\t']          {lexer lexbuf}   (* lexème éludé ; la fonction est rappelée récursivement *)

  | '\n'                {Leol}           (* renvoi d'une constante désignant *)
  | '('                 {Llpar}          (* la sorte de lexème reconnu *)
  | ')'                 {Lrpar}
  | '!'| "non"          {Lnot}
  | "=>"                {Linvolve}
  | "<=>"               {Lequiv}
  | "&&" | "et"         {Land}
  | "||" | "ou"         {Lor}
  | "vrai"              {Ltrue}
  | "faux"              {Lfalse}
  | ['a'-'z' 'A'-'Z']+  {Lident (Lexing.lexeme lexbuf)}
                                         (* dans le cas d'un identificateur, on renvoie aussi*)
                                         (* la chaîne de caractères correspondante *)