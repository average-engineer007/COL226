type bop = Add | Sub | Mul | Div ;;
type uop = UnopMinus | NOT ;;
type blop = AND | OR ;;


type exp = 
  | Int of int
  | Bool of bool
  | V of string 
  | Abs of string * exp 
  | App of exp * exp
  | Boolop of blop * exp * exp
  | BinOp of bop * exp * exp
  | UnOp  of uop * exp
  | Grtr of exp * exp
  | If_Else of exp * exp * exp
  | Eq of exp * exp
;;


type opcode = 
  | LOOKUP of string 
  | APP 
  | MKCLOS of string * opcode list 
  | RET
  | PLUS 
  | TIMES 
  | SUB
  | DIV
  | AND 
  | OR 
  | NOT 
  | NEG
  | Loadi of int 
  | Loadb of bool
  | EQ
  | Greater
  | IfElse
;;

let rec compile exp= match exp with
  | V x -> [LOOKUP x]
  | Int i -> [Loadi i]
  | Bool b -> [Loadb b]
  | BinOp (o,e1,e2) ->
    let compiled_e1=compile e1 in
    let compiled_e2 =compile e2 in
    compiled_e1 @ compiled_e2 @
      (match o with
      | Add -> [PLUS]
      | Sub -> [SUB]  
      | Mul -> [TIMES]
      | Div -> [DIV] )
  | Boolop (o,e1,e2) ->
    let compiled_e1=compile e1 in
    let compiled_e2 =compile e2 in
    compiled_e1 @ compiled_e2 @
      (match o with
      | AND -> [AND]
      | OR -> [OR])
  | UnOp (o,e) -> 
    let compiled_e = compile e in
    compiled_e @
      (match o with
      | UnopMinus -> [NEG]
      | NOT -> [NOT])
  | App (e1,e2) ->
    let compiled_e1=compile e1 in
    let compiled_e2 =compile e2 in
    compiled_e1 @ compiled_e2 @ [APP]
  | Abs (x,e) ->
      let compiled_expression = compile e in
      [MKCLOS (x, compiled_expression @ [RET])]
  | Grtr (e1,e2) ->
    let compiled_e1=compile e1 in
    let compiled_e2 =compile e2 in
    compiled_e1 @ compiled_e2 @ [Greater]
  | Eq (e1,e2) ->
    let compiled_e1=compile e1 in
    let compiled_e2 =compile e2 in
    compiled_e1 @ compiled_e2 @ [EQ]
  | If_Else (e1,e2,e3) ->
    let compiled_e1=compile e1 in
    let compiled_e2 =compile e2 in
    let compiled_e3 =compile e3 in
    compiled_e1 @ compiled_e2 @ compiled_e3 @ [IfElse]
;;

type ans = Int of int | Bool of bool;;
type table = (string * ans )  list ;;
type c_list = opcode list;;
type clos = Clos of string * opcode list * table ;;
type s_el =Ans of ans | Clos of string * opcode list * table ;;
type stack = s_el list  ;;
type d_el = stack*table*c_list;;
type dump = d_el list;;

type secd = stack*table*c_list*dump;; 

let rec lookup_variable x table =
  match table with
  | [] -> failwith ("Variable " ^ x ^ " not found in table")
  | (var,value) :: rest ->
      if var = x then value else lookup_variable x rest
;;

let rec add_variable x v table =
  match table with
  | [] -> [(x,v)]
  | (var,value) :: rest ->
      if var = x then add_variable x v rest else (var,value) :: add_variable x v rest
;;

let transition secd =
  match secd with
  | (s,t,MKCLOS (x, op_list) :: c',d) ->
    let closure = Clos (x, op_list, t) in
    (closure :: s, t, c', d)
  | (s,t,LOOKUP x :: c',d) ->
      let a = lookup_variable x t in
      ( Ans a:: s, t, c', d)    
  | (s,t,APP :: c',d) ->
        (match s with
        | (Ans a) :: closure :: s' ->
            (match closure with
            | Clos (x, op_list, t') ->
                let t_with_var = add_variable x a t' in
                ([], t_with_var, op_list, (s', t, c') :: d)
            | _ -> failwith "Invalid closure")
        | _ -> failwith "Invalid stack")
  | (s,t,RET:: c',d)  ->
        (match d with
        | (s', t', c'') :: d' ->
          (match s with
          | a :: s'' -> (a :: s', t', c'', d')
          | _ -> failwith "invalid stack")
        | _ -> failwith "Invalid dump")
  | (Ans (Int a) :: Ans (Int b) :: s',t,PLUS :: c',d ) ->
    (Ans (Int (a+b)) :: s', t, c', d)
  | (Ans (Int a) :: Ans (Int b) :: s', t, TIMES :: c', d) ->
    (Ans (Int (a * b)) :: s', t, c', d)
  | (Ans (Int a) :: Ans (Int b) :: s', t, SUB :: c', d) ->
    (Ans (Int (b - a)) :: s', t, c', d)
  | (Ans (Int a) :: Ans (Int b) :: s', t, DIV :: c', d) ->
    (Ans (Int (b / a)) :: s', t, c', d)
  | (Ans (Int a) :: s', t, NEG :: c', d) ->
    (Ans (Int (-a)) :: s', t, c', d)
  | (Ans (Bool a) :: s', t, NOT :: c', d) ->
    (Ans (Bool (not a)) :: s', t, c', d)
  | (Ans (Bool a) :: Ans (Bool b) :: s', t, AND :: c', d) ->
    (Ans (Bool (a && b)) :: s', t, c', d)
  | (Ans (Bool a) :: Ans (Bool b) :: s', t, OR :: c', d) ->
    (Ans (Bool (a || b)) :: s', t, c', d)
  | (s,t,Loadi x :: c',d) ->
    ( Ans (Int x):: s, t, c', d) 
  | (s,t,Loadb x :: c',d) ->
    ( Ans (Bool x):: s, t, c', d) 
  | (Ans (Int a) :: Ans (Int b) :: s',t,Greater :: c',d ) ->
    let ret = if b>a then true else false in 
    (Ans (Bool ret) :: s', t, c', d)
  | (Ans (Int a) :: Ans (Int b) :: s',t,EQ :: c',d ) ->
    let ret = if a=b then true else false in
    (Ans (Bool ret) :: s', t, c', d)
  | (Ans c :: Ans b :: Ans(Bool a) :: s',t,IfElse:: c',d ) ->
    let ret = if a then b else c in
      (Ans ret :: s', t, c', d)
  | _ -> failwith "Invalid transition"
;;



let rec string_of_oplist op_list =
  match op_list with
  | [] -> ""
  | op :: rest ->
      (match op with
      | LOOKUP x -> "LOOKUP " ^ x
      | APP -> "APP"
      | MKCLOS (x, op_list') -> "MKCLOS (" ^ x ^ ", " ^ string_of_oplist op_list' ^ ")"
      | RET -> "RET"
      | PLUS -> "PLUS"
      | TIMES -> "TIMES"
      | SUB -> "SUB"
      | DIV -> "DIV"
      | AND -> "AND"
      | OR -> "OR"
      | NOT -> "NOT"
      | NEG -> "NEG"
      | Loadi i -> "Loadi " ^ string_of_int i
      | Loadb b -> "Loadb " ^ string_of_bool b
      | EQ -> "EQ"
      | Greater -> "Greater"
      | IfElse -> "IfElse"
      ) ^ (if rest <> [] then "; " ^ string_of_oplist rest else "")

let string_of_ans = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b

let string_of_stack s =
  "Stack: " ^ String.concat ", " (List.map (function | Ans a -> string_of_ans a | Clos (x, _, _) -> "Closure(" ^ x ^ ")") s)

let string_of_table t =
  "Table: " ^ String.concat ", " (List.map (fun (x, a) -> x ^ " -> " ^ string_of_ans a) t)

let string_of_dump d =
  "Dump: " ^ String.concat " , " (List.map (fun (s, t, c) -> "{ " ^ string_of_stack s ^ " ; " ^ string_of_table t ^ " ; " ^ "Code: " ^string_of_oplist c ^ " }") d)

let string_of_secd (s, t, c, d) =
  string_of_stack s ^ "\n" ^
  string_of_table t ^ "\n" ^
  "Code: " ^ string_of_oplist c ^ "\n" ^
  string_of_dump d ^ "\n" 



let test_compile exp =
  let cc = compile exp in
  let outp = string_of_oplist cc in
  print_endline ("Compiled code: " ^ outp)
;;




let rec run_secd secd =
  print_endline (string_of_secd secd);
  match secd with
  | (Ans a::s', _, [], _) ->
    print_endline ("The final answer is " ^ string_of_ans a);
    print_endline (String.make 100 '*') 
  | (Clos (a, _, _) :: s', _, [], _) ->
      print_endline ("The final answer is a closure");
      print_endline (String.make 100 '*')  
  | _ -> run_secd (transition secd)
;;

let test_secd ce =
  let initial_secd_state = ([], [], ce, []) in  
    run_secd initial_secd_state
;;


let () =
  (* let test_exp = App(Abs("x",BinOp(Add,Int 2,V "x")),Int 2) (* Example expression *)
  in
  test_compile test_exp  *)
  let test_exp = App(Abs("x",BinOp(Add,Int 3,V "x")),BinOp(Mul,App(Abs("x",BinOp(Add,Int 2,V "x")),Int 2),Int 3)) in
    let compiled_code = compile test_exp in
      test_secd compiled_code
;;
 
(* let test_add_variable () =
  let table = [("x", Int 1); ("y", Int 2); ("z", Int 3)] in
  let updated_table = add_variable "c" (Bool true) table in
  let p=string_of_table updated_table in 
  print_endline (p)

;;

test_add_variable () *)


(*
some test cases 
BinOp(Mul,App(Abs("x",BinOp(Add,Int 2,V "x")),Int 2),Int 3)
BinOp(Add,Int 3,BinOp(Mul,Int 4,UnOp(UnopMinus,Int 2))) 
Boolop(AND,Bool true,UnOp(NOT,Bool false))
App(Abs("x",BinOp(Add,3,V "x")),BinOp(Mul,App(Abs("x",BinOp(Add,Int 2,V "x")),Int 2),Int 3))
If_Else(Bool true,Int 2,Int 3)
If_Else(Grtr(App(Abs("x",BinOp(Add,Int 3,V "x")),Int 5),Int 6),Int 5,Bool true)
App(Abs("x",App(Abs("y",BinOp(Add,V "x",V "y")),Int 1)),Int 2)

*)

let test_cases = [
  (* Test case 1 *)
  (* ( [], [("x", Int(1))], compile (V "y"), []) ; *)
  
  (* Test case 2 *)
  ( [], [("x", Int(1))], compile (V "x"), []) ;
  
  (* Test case 4 *)
  ( [], [("x", Int(1))], compile (App(Abs("x", V "x"), V "x")), []) ;
  
  (* Test case 5 *)
  ( [], [("x", Int(1)); ("y", Int(2))], compile (App(Abs("y", V "y"), V "x")), []) ;
  
  (* Test case 6 *)
  ( [], [("x", Int(1)); ("y", Int(2))], compile (App(Abs("y", Abs("x", V "y")), V "x")), []) ;
  
  (* Test case 7 *)
  ( [], [("x", Int(1)); ("y", Int(2))], compile (App(Abs("x", App(Abs("y", V "y"), V "x")), V "x")), [])
]

let test_and_print_result (s, t, c, d) =
  let initial_secd_state = (s, t, c, d) in  
    run_secd initial_secd_state

let run_test_cases test_cases =
  List.iter (fun (s, t, c, d) -> test_and_print_result (s, t, c, d)) test_cases

(* let () =
  run_test_cases test_cases *)

