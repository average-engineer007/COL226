open Lexer;;
open Parser;;
open Interpreter;;
open Printf;;

if Array.length Sys.argv < 2 then begin
  print_string "Input file not provided.\nExiting...\n";
  exit 0;
end;;

if Array.length Sys.argv > 2 then begin
  print_string "Too many arguments.\nExiting...\n";
  exit 0;
end;;

let fstream = open_in Sys.argv.(1);;
let init_prog = Parser.main Lexer.token (Lexing.from_channel fstream);;

(* this is important do not change this for example if i comment this and the file is 
  f(a).
f(b).
g(X,Y):-f(X),f(Y).
and running g(Y,X) gives wrong output *)
(* let _ = checkProgram init_prog;; *)
let prog = modifyInitialProg init_prog 1;;
printf "Parsed program: %s\n" (Interpreter.string_of_program init_prog);

print_string "Program loaded successfully\n";;

try
  while(true) do
    print_string "?- ";
    let line = read_line() in
    if line = "halt." then exit 0
    else try
      let g = Parser.goal Lexer.token (Lexing.from_string line) in
      match (interpret_goal prog g) with
          (true, _) -> print_string "true.\n"
        | (false, _) -> print_string "false.\n"
    with e -> Printf.printf "%s\n" (Printexc.to_string e)
  done

with _ -> print_string "\n% halt\n"
