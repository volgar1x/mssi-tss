open Types ;;
open Eval ;;
open Parsing ;;
open Lexing ;;

let load_stdlib () =
  let ic = open_in "./stdlib.lambda" in

  let ev ctx buf no =
    try
      let expr = Parser.line Lexer.lexer buf in
      let (_, new_ctx) = eval expr ctx in
      new_ctx
    with
    | Parse_error ->
      raise (Eval_exn ("Parse Error on " ^ (string_of_int no) ^ ":" ^ (string_of_int buf.lex_curr_pos) ^ " `" ^ (lexeme buf) ^ "'"))
    | Failure reason ->
      raise (Eval_exn ("Parse Error at line " ^ (string_of_int no) ^ " `" ^ (lexeme buf) ^ "' because: " ^ reason))
  in

  let rec aux acc no =
    try
      let line = input_line ic in
      if (String.length (String.trim line)) > 0 then
        let buf = Lexing.from_string (line ^ "\n") in
        aux (ev acc buf no) (no + 1)
      else
        aux acc (no + 1)
    with
    | End_of_file ->
      close_in ic;
      acc
    | e ->
      close_in ic;
      raise e
  in

  aux [] 1
  ;;
