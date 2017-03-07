type token =
  | Leol
  | Loparen
  | Lcparen
  | Llambda
  | Ldot
  | Llet
  | Leq
  | Lident of (string)

val line :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.expression
