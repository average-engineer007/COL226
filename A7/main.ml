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


type clos= Clos of exp*table and table = (string * clos)  list ;;

type krivine = clos * (clos list);;


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


let rec string_of_exp e =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | V x -> x
  | Abs (x, e') -> "(λ" ^ x ^ "." ^ string_of_exp e' ^ ")"
  | App (e1, e2) -> "APP(" ^ string_of_exp e1 ^ " ; " ^ string_of_exp e2 ^ ")"
  | Boolop (bop, e1, e2) -> string_of_blop bop ^ "(" ^ string_of_exp e1 ^ "," ^ string_of_exp e2 ^ ")"
  | BinOp (bop, e1, e2) -> string_of_bop bop ^ "(" ^ string_of_exp e1 ^ "," ^ string_of_exp e2 ^ ")"
  | UnOp (uop, e') -> string_of_uop uop ^ "(" ^ string_of_exp e' ^ ")"
  | Grtr (e1, e2) -> "(" ^ string_of_exp e1 ^ " > " ^ string_of_exp e2 ^ ")"
  | If_Else (cond, e1, e2) -> "(if " ^ string_of_exp cond ^ " then " ^ string_of_exp e1 ^ " else " ^ string_of_exp e2 ^ ")"
  | Eq (e1, e2) -> "(" ^ string_of_exp e1 ^ " = " ^ string_of_exp e2 ^ ")"
and string_of_bop bop =
  match bop with
  | Add -> "ADD"
  | Sub -> "SUB"
  | Mul -> "MUL"
  | Div -> "DIV"
and string_of_uop uop =
  match uop with
  | UnopMinus -> "NEG"
  | NOT -> "NOT"
and string_of_blop blop =
  match blop with
  | AND -> "AND"
  | OR -> "OR"

let rec string_of_clos c =
  match c with
  | Clos (e, t) -> string_of_exp e ^ ", " ^ string_of_table t
and string_of_table t =
  "Table:[ " ^ String.concat ", " (List.map (fun (x, a) -> x ^ " -> {" ^ string_of_clos a ^ "}") t) ^ "]"


let print_krivine_state state = 
  let (clos, stack) = state in
  print_endline ("Closure: " ^ string_of_clos clos);
  print_endline ("Stack: " ^ String.concat ", " (List.map (fun c -> "Clos(" ^ string_of_clos c ^ ")") stack))
;;

let rec solve krivine = 
  print_endline "Current state:";
  print_krivine_state krivine;
  match krivine with
  | (Clos(Int i , y),s) -> krivine
  | (Clos(Bool b , y),s) -> krivine
  | (Clos(Abs(x,e),y),[]) -> krivine
  | _ -> solve (transition krivine)

and transition krivine = match krivine with
  | (Clos (App (e1, e2) ,y),s) -> (Clos(e1,y),Clos(e2,y)::s)
  | (Clos(Abs(x,e),y),cl::s) -> (Clos(e,add_variable x cl y),s)
  | (Clos(V x,y),s) -> (lookup_variable x y,s)
  | (Clos(UnOp(op, e), y), s) -> 
    let (e_solved, s') = solve (Clos(e, y), []) in 
      (match (op, e_solved) with
      | (UnopMinus, Clos(Int x, _)) -> (Clos(Int (-x), y), s)
      | (NOT, Clos(Bool b, _)) -> (Clos(Bool (not b), y), s)
      | _ -> failwith "Unary operation is not applicable to the given value")

  | (Clos(Boolop(bop, e1, e2), y), s) -> 
    let (e1_solved, s') = solve (Clos(e1, y), []) in 
    let (e2_solved, s'') = solve (Clos(e2, y), []) in 
      (match (bop, e1_solved, e2_solved) with
      | (AND, Clos(Bool b1, _), Clos(Bool b2, _)) -> (Clos(Bool (b1 && b2), y), s)
      | (OR, Clos(Bool b1, _), Clos(Bool b2, _)) -> (Clos(Bool (b1 || b2), y), s)
      | _ -> failwith "Boolean operation is not applicable to the given values")
  | (Clos(Grtr(e1, e2), y), s) ->
    let (e1_solved, s') = solve (Clos(e1, y), []) in 
    let (e2_solved, s'') = solve (Clos(e2, y), []) in 
      (match (e1_solved, e2_solved) with
      | (Clos(Int a, _), Clos(Int b, _)) -> (Clos(Bool (a > b), y), s)
      | _ -> failwith "Comparison is not applicable to the given values")
  | (Clos(If_Else(cond, e1, e2), y), s) ->
    let (cond_solved, s') = solve (Clos(cond, y), []) in 
      (match cond_solved with
      | Clos(Bool true, _) -> (Clos(e1, y), s)
      | Clos(Bool false, _) -> (Clos(e2, y), s)
      | _ -> failwith "Condition must evaluate to a boolean value")
  | (Clos(Eq(e1, e2), y), s) ->
    let (e1_solved, s') = solve (Clos(e1, y), []) in 
    let (e2_solved, s'') = solve (Clos(e2, y), []) in 
      (match (e1_solved, e2_solved) with
      | (Clos(Int a, _), Clos(Int b, _)) -> (Clos(Bool (a = b), y), s)
      | (Clos(Bool b1, _), Clos(Bool b2, _)) -> (Clos(Bool (b1 = b2), y), s)
      | _ -> failwith "Equality comparison is not applicable to the given values")

  | (Clos(BinOp(op, e1, e2), y), s) ->
    let e1_solved = solve (Clos(e1, y), []) in 
    let e2_solved = solve (Clos(e2, y), []) in 
    (match (e1_solved, e2_solved) with
      | (Clos(Int a, _), _), (Clos(Int b, _), _) -> 
        (match op with
          | Add -> (Clos(Int (a + b), y), s)
          | Sub -> (Clos(Int (a - b), y), s)
          | Mul -> (Clos(Int (a * b), y), s)
          | Div -> (Clos(Int (a / b), y), s))
      | _ -> failwith "Binary operation is not applicable to non-integer values")
  | _ -> failwith "Invalid krivine configuration"
;;

let test_cases = [
  (Clos(V ("z"), [( "z", Clos(Int(3), []))]), []) ;
  
  (Clos(BinOp(Add, BinOp(Add,Int(2),Int(3)),BinOp(Add,Int(2),Int(3))), []), []) ;
  
  (Clos(App(Abs("x",BinOp(Add,V("x"),Int(1))),Int(2)), []), []) ;
  
  (Clos(App(Abs("x", BinOp(Mul,V("x"),BinOp(Add,V("x"),Int(1)))),Int(2)), []), []) ;
  
  (Clos(App(Abs("x", App(Abs("d",BinOp(Mul,V("d"),Int(2))),Int(2))),Int(2)), []), []) ;
  
  (Clos(If_Else(Grtr(Int(8),Int(2)),App(Abs("x", BinOp(Div,V("x"),Int(2))),Int(2)),App(Abs("x", BinOp(Mul,V("x"),BinOp(Add,V("x"),Int(1)))),Int(2))), []), []) ;

  (Clos(If_Else(Grtr(Int(8),Int(2)),BinOp(Add,Int(1), Int(2)), BinOp(Div,Int(9), Int(0))), []), [])   
]  

let () =
List.iter (fun initial_state ->
  let final_state = solve initial_state in
  match final_state with
  | (result, _) -> 
    print_endline("Result: " ^ string_of_clos result);
    print_newline ();  
    print_newline ()  
) test_cases
;;


(* let test_and_print_result (clos, stack) =
  let krivine_result = solve (clos, stack) in
  print_endline "Krivine result:";
  print_krivine_state krivine_result

let run_test_cases test_cases =
  List.iter test_and_print_result test_cases

let () =
  run_test_cases test_cases *)


(* test_cases
let initial_state = (Clos (BinOp(Add,Int 1,Int 2), []), []) in 
let initial_state = (Clos(BinOp(Mul,App(Abs("x",BinOp(Add,Int 2,V "x")),Int 2),Int 3), []), []) in  *)


(* BinOp(Add,Int 3,BinOp(Mul,Int 4,UnOp(UnopMinus,Int 2))) 
Boolop(AND,Bool true,UnOp(NOT,Bool false))
App(Abs("x",BinOp(Add,3,V "x")),BinOp(Mul,App(Abs("x",BinOp(Add,Int 2,V "x")),Int 2),Int 3))
If_Else(Bool true,Int 2,Int 3)
If_Else(Grtr(App(Abs("x",BinOp(Add,Int 3,V "x")),Int 5),Int 6),Int 5,Bool true)
App(Abs("x",App(Abs("y",BinOp(Add,V "x",V "y")),Int 1)),Int 2) *)

(* some cases that are specific to being lazy
(Clos(App(Abs("x",BinOp(Add,Int 2,Int 3)),Bool true), []), [])
(Clos(App(Abs("x",Abs("y",BinOp(Add,V "x",V "y"))),Int 3), []), []) 
(Clos (If_Else (App(Abs("x",Grtr(Int 2,Int 3)),Int 3), Int 1, BinOp (Add, Int 2, Int 3)), []),[]) *)

(* CLtype(V("z"), [(V("z"),CLtype(Int(3), []))]);;
Clos(BinOp(PLUS, BinOp(PLUS,Int 2 ,Int 3 ),BinOp(PLUS,Int 2 ,Int 3 )), []);; *)
