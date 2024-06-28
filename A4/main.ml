open Printf
open Ast
open Parser


let main =
  let lexbuf = Lexing.from_channel stdin in
  let res =
    try Parser.main Lexer.token lexbuf
    with
    | Lexer.Error c ->
       fprintf stderr "Lexical error at line %d: Unknown character '%c'\n"
         lexbuf.lex_curr_p.pos_lnum c;
       exit 1
    | Parsing.Parse_error ->
        fprintf stderr "Parse error at line %d:\n" lexbuf.lex_curr_p.pos_lnum;
        exit 1
  in
  let output_file = "output.txt" in
  let oc = open_out output_file in
  fprintf oc "%s\n" (pprint_expr_list res);
  close_out oc;
  printf "Output written to %s\n" output_file


(* try Parser.main Lexer.token lexbuf
with
| Lexer.Error c ->
   fprintf stderr "Lexical error at line %d: Unknown character '%c'\n"
     lexbuf.lex_curr_p.pos_lnum c;
   exit 1
| Parser.Error ->
   fprintf stderr "Parse error at line %d:\n" lexbuf.lex_curr_p.pos_lnum;
   exit 1 *)