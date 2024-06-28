open Printf

type bop = BopAdd | BopSub | BopMul | BopDiv
type uop = UnopMinus | NOT
type fname = Fname of string
type bconst = Booltrue | Boolfalse
type blop = AND | OR 

type expr =
  | Int of int
  | Boolconst of bconst
  | Boolop of blop * expr * expr
  | BinOp of bop * expr * expr
  | UnOp  of uop * expr
  (* | Let   of string * expr * expr *)
  | Var   of string
  | Fact of expr
  | Goal of expr list
  | Rule of expr * expr
  | Arguments of expr list
  | Func of fname * expr 




let pprint_bop = function
  | BopAdd -> "+"
  | BopSub -> "-"
  | BopMul -> "*"
  | BopDiv -> "/"

let pprint_blop = function
  | AND -> "AND"
  | OR -> "OR"

let pprint_bconst = function
  | Booltrue -> "true"
  | Boolfalse -> "false"
  
let pprint_uop = function
  | UnopMinus -> "-"
  | NOT -> "NOT"

let pprint_fname = function 
  | Fname(s) -> "Fname(" ^ s ^ ")"

let rec pprint_expr = function
  | Int(i) -> sprintf "Int(%d)" i
  | Boolconst(bconst) -> pprint_bconst bconst
  | Fact(e1) ->
      "Fact(\n\t" ^ pprint_expr e1 ^ "\n)"
  | BinOp(bop, e1, e2) ->
     "BinOp(" ^ pprint_bop bop ^ ", " ^ pprint_expr e1 ^ "," ^ pprint_expr e2 ^ ")"

  | UnOp(uop, e) ->
     "UnOp(" ^ pprint_uop uop ^ ", " ^ pprint_expr e ^ ")"
  | Boolop(blop,e1,e2) ->
    "BoolOp(" ^ pprint_blop blop ^ ", " ^ pprint_expr e1 ^ "," ^ pprint_expr e2 ^ ")"

  (* | ELet(x, e1, e2) -> "ELet(" ^ x ^ ", " ^ pprint_expr e1 ^ ",\n" ^ pprint_expr e2 ^ ")" *)
  | Var(x) -> "Var(" ^ x ^ ")"
  | Goal(elist) ->
    "Goal : [ " ^ String.concat " ; " (List.map pprint_expr elist) ^ " ]"
  | Rule(e1,e2) ->
    "Rule(\n\tHead :  " ^ pprint_expr e1 ^ "\n\t" ^ pprint_expr e2 ^ "\n)"
  | Arguments(elist) ->
    "Arguments(" ^ String.concat " , " (List.map pprint_expr elist) ^ ")"
  | Func(f,e) ->
    "Function{ " ^ pprint_fname f ^ " , " ^  pprint_expr e ^ " }" 

type expr_list = ExprList of expr list
let rec pprint_expr_list (ExprList expr_list) =
  match expr_list with
  | [] -> ""  (* Empty list *)
  | [expr] -> pprint_expr expr  (* Single expression *)
  | expr :: rest -> pprint_expr expr ^ "\n \n" ^ pprint_expr_list (ExprList rest)  (* Expression followed by a newline and space, followed by the rest of the list *)
