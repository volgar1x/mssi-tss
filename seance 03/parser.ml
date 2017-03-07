type token =
  | Leol
  | Loparen
  | Lcparen
  | Llambda
  | Ldot
  | Llet
  | Leq
  | Lident of (string)

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Types ;;
# 16 "parser.ml"
let yytransl_const = [|
  257 (* Leol *);
  258 (* Loparen *);
  259 (* Lcparen *);
  260 (* Llambda *);
  261 (* Ldot *);
  262 (* Llet *);
  263 (* Leq *);
    0|]

let yytransl_block = [|
  264 (* Lident *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\003\000\000\000"

let yylen = "\002\000\
\002\000\003\000\004\000\004\000\001\000\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\008\000\000\000\
\000\000\000\000\000\000\000\000\001\000\006\000\002\000\000\000\
\000\000\004\000\003\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000"

let yysindex = "\003\000\
\255\254\000\000\255\254\000\255\255\254\000\000\000\000\008\255\
\255\254\007\255\006\255\005\255\000\000\000\000\000\000\255\254\
\255\254\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\255\
\000\000\013\255\000\000\013\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\253\255\000\000"

let yytablesize = 21
let yytable = "\010\000\
\003\000\012\000\004\000\001\000\005\000\014\000\006\000\011\000\
\013\000\015\000\016\000\017\000\018\000\019\000\007\000\000\000\
\007\000\000\000\007\000\000\000\007\000"

let yycheck = "\003\000\
\002\001\005\000\004\001\001\000\006\001\009\000\008\001\008\001\
\001\001\003\001\005\001\007\001\016\000\017\000\002\001\255\255\
\004\001\255\255\006\001\255\255\008\001"

let yynames_const = "\
  Leol\000\
  Loparen\000\
  Lcparen\000\
  Llambda\000\
  Ldot\000\
  Llet\000\
  Leq\000\
  "

let yynames_block = "\
  Lident\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 20 "parser.mly"
                                   (_1)
# 90 "parser.ml"
               : Types.expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 23 "parser.mly"
                                   (_2)
# 97 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 24 "parser.mly"
                                   (Assignation (_2, _4))
# 105 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 25 "parser.mly"
                                   (Function (_2, _4, []))
# 113 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 26 "parser.mly"
                                   (Variable (_1))
# 120 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr2) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 27 "parser.mly"
                                   (Application (_1, _2))
# 128 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 29 "parser.mly"
             (_1)
# 135 "parser.ml"
               : 'expr2))
(* Entry line *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let line (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Types.expression)
;;
