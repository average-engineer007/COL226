open Printf
open Token


let pp_token = function
  | KEYWORD s -> sprintf "KEYWORD(%s)" s
  | ARITH_OP s -> sprintf "ARITH_OP(%s)" s
  | INT(i) -> sprintf "INT(%d)" i
  | IDENTIFIER id -> sprintf "IDENTIFIER(%s)" id
  | BOOL_CONST b -> sprintf "BOOL_CONST(%b)" b
  | BOOL_OP s -> sprintf "BOOL_OP(%s)" s
  | COMPARISON_OP s -> sprintf "COMPARISON_OP(%s)" s
  | ASSIGNMENT_OP -> "ASSIGNMENT_OP(=)"
  | STRING_CONST scons -> sprintf "STRING_CONST(%s)" scons
  | STRING_OP s -> sprintf "STRING_OP(%s)" s
  | COMMA -> "COMMA"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | ERRORT er -> sprintf "ERROR(%s)" er
  | ERROR s -> sprintf "ERROR(%s)" s
  | EOF -> "EOF"


(* let get_token_list lexbuf=
  let rec work acc =
    match Lexer.token lexbuf with
    | EOF -> acc
    | t -> work (t::acc)
  in List.rev (work [])



let main=
  let lexbuf = Lexing.from_string "( _abc1_10 * 2,_ true and ` <=" in
  let token_list =  get_token_list lexbuf in
  List.map pp_token token_list |> List.iter (printf"%s\n") *)

   
(* let get_token_list lexbuf =
  let rec work acc =
    match Lexer.token lexbuf with
    | EOF -> List.rev acc
    | tok ->
        printf "%s\n" (pp_token tok);
        if tok = EOF then
          List.rev (tok :: acc)
        else
          work (tok :: acc)
  in work []

let main =
  let lexbuf = Lexing.from_string "2+" in
  let _ =  get_token_list lexbuf in
  () *)




let get_token_list lexbuf =
  let rec work acc =
    match Lexer.token lexbuf with
    | EOF -> List.rev acc
    | tok ->
        (* printf "%s\n" (pp_token tok); *)
        if tok = EOF then
          List.rev (tok :: acc)
        else
          work (tok :: acc)
  in work []

let get_token_list_all strings =
  List.map (fun str ->
      let lexbuf = Lexing.from_string str in
      get_token_list lexbuf) strings

let main =
  let strings = ["2 + 3"; "if x > 0 then y else z"; "true && false" ; "Abcd" ; "1sdf" ; "25<=2" ; "a34_3yd y000123 afe&& 'bcs()[]{}@#242343 Y0124  00123true"] in
  let token_lists = get_token_list_all strings in


  List.iter (fun (str, tokens) ->
    Printf.printf "Input string: %s\n" str;
    List.iter (fun token ->
        Printf.printf "%s\n" (pp_token token)) tokens;
    print_endline "---") (List.combine strings token_lists)



    (* let strings = ["2 + 3"; 
    "if x > 0 then y else z"; 
    "true && false" ;
    "Abcd" ; 
    "1sdf" ; 
    "25<=2" ;
    "a34_3yd y000123 afe&& 'bcs()[]{}@#242343 Y0124  00123true" ;
    "\"abcd\""
    "\"hello"
    "1+ 234if0"
    "hello %0012"
      ]  *)



