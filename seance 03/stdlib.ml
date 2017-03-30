open Types ;;
open Eval ;;
open Parsing ;;
open Lexing ;;
open Dict ;;
open Typechecker ;;

let load_stdlib () =
  let ic = open_in "./stdlib.lambda" in

  let ev gamma ctx buf no =
    try
      let expr = Parser.line Lexer.lexer buf in
      let exprT = type_of_expression gamma expr in
      let new_gamma = (match expr with
        | Bind (varname, _, Unit) ->
          { next_param = gamma.next_param;
            context = dict_put varname exprT gamma.context;
          }

        | _ ->
          gamma
      ) in
      let (_, new_ctx) = eval expr ctx in
      (new_gamma, new_ctx)
    with
    | Parse_error ->
      raise (Eval_exn ("Parse Error on " ^ (string_of_int no) ^ ":" ^ (string_of_int buf.lex_curr_pos) ^ " `" ^ (lexeme buf) ^ "'"))
    | Failure reason ->
      raise (Eval_exn ("Parse Error at line " ^ (string_of_int no) ^ " `" ^ (lexeme buf) ^ "' because: " ^ reason))
    | Type_exn reason ->
      raise (Eval_exn ("Type Mismatch at line " ^ (string_of_int no) ^ " because: " ^ reason))
  in

  let rec aux gamma acc no =
    try
      let line = input_line ic in
      if (String.length (String.trim line)) > 0 then
        let buf = Lexing.from_string (line ^ "\n") in
        let (new_gamma, new_ctx) = ev gamma acc buf no in
        aux new_gamma new_ctx (no + 1)
      else
        aux gamma acc (no + 1)
    with
    | End_of_file ->
      close_in ic;
      (gamma, acc)
    | e ->
      close_in ic;
      raise e
  in

  aux empty_gamma [] 1
  ;;
