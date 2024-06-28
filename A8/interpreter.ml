module StringSet = Set.Make(String);;
(* exception Ununifiable of string;; *)
exception Ununifiable
exception NotFound
exception InvalidProgram
exception NotPossible
;;


type symbol = Symbol of string
type term = V of string | Int of int | C of {node: symbol; children: term list};; 
type atom = A of symbol * (term list)
type head = H of atom 
type body = B of atom list 
type clause = Fact of head | Rule of head * body
type program = clause list
type goal = G of atom list
let rec string_of_symbol = function
  | Symbol s -> s

let rec string_of_term = function
  | V v -> v
  | Int i -> string_of_int i
  | C { node; children } ->
    let children_str = List.map string_of_term children |> String.concat ", " in
    string_of_symbol node ^ "(" ^ children_str ^ ")"

let string_of_atom (A (symbol, terms)) =
  let terms_str = List.map string_of_term terms |> String.concat ", " in
  string_of_symbol symbol ^ "(" ^ terms_str ^ ")"

let string_of_head = function
  | H atom -> string_of_atom atom

let string_of_body = function
  | B atoms ->
    List.map string_of_atom atoms |> String.concat ", "

let string_of_clause = function
  | Fact head -> string_of_head head ^ "."
  | Rule (head, body) -> string_of_head head ^ " :- " ^ string_of_body body ^ "."

let string_of_program program =
  List.map string_of_clause program |> String.concat "\n"



let rec map f l = match l with
    [] -> []
  | x::xs -> (f x)::map f xs
;;


type signature = (symbol * int) list
let sample_signature = [(Symbol "A", 2); (Symbol "B", 2); (Symbol "C", 2);] 

let rec symbol_exists symbol signature =
  match signature with
  | [] -> false
  | (s, _) :: rest -> s = symbol || symbol_exists symbol rest

let rec check_sig_helper signature =
  match signature with
  | [] -> true
  | (symbol, arity) :: rest ->
      (arity >= 0) && not (symbol_exists symbol rest) && check_sig_helper rest

let check_sig signature =
  check_sig_helper signature
  
let rec find_arity symbol signature =
  match signature with
    | [] -> -1
    | (s, a) :: rest -> if s=symbol then a else find_arity symbol rest

let rec helper signature t =
  match t with
  | V _ -> true
  | Int _ -> true
  | C { node = n; children = children } ->
    if find_arity n signature = List.length children then
      List.fold_left (fun acc child -> acc && helper signature child) true children
    else
      false

let wfterm signature t =
  check_sig signature && helper signature t 

let sample_term_valid =
  C {
    node = Symbol "A";
    children = [
      V "Leaf 1";
      C {
        node = Symbol "B";
        children = [
          C{
            node = Symbol "A";
            children=[ V "Leaf 2"]
          }
          ; V "Leaf 3"]
      }
    ]
  }

let sample_term_invalid =
  C {
    node = Symbol "C";
    children = [
      V "Leaf 1";
      C {
        node = Symbol "D"; 
        children = [V "Leaf 2"]
      }
    ]
  }



let rec ht t = match t with
  | V _ -> 0
  | Int _ -> 0
  | C {node;children} ->
    if List.length children = 0 then
      1 
    else
      1 + List.fold_left (fun acc child -> max acc (ht child)) 0 children


let rec size t = match t with
  | V _ -> 1
  | Int _ -> 1
  | C { children = children } ->
    1 + List.fold_left (fun acc child -> acc + size child) 0 children




let rec vars t =
  match t with
  | V v -> StringSet.singleton v  
  | C { children = children } ->
    List.fold_left (fun acc child -> StringSet.union acc (vars child)) StringSet.empty children
  | _ -> StringSet.empty

let print_set set =
  let elements_list = StringSet.elements set in
  List.iter (fun x -> print_string x; print_string " ") elements_list;
  print_newline ()


let rec mirror_term t =
  match t with
  | V _ -> t  
  | Int _ -> t
  | C { node = n; children = children } ->
    let mirrored_children = List.map mirror_term (List.rev children) in
    C { node = n; children = mirrored_children }





let rec print_term t =
  match t with
  | V v -> print_string v 
  | Int i -> print_int i
  | C { node = n; children = children } ->
    print_string (match n with Symbol s -> s);
    print_string " [";
    List.iter (fun child -> print_term child; print_string ", ") children;
    print_string "]"

let sample_term =
  C {
    node = Symbol "A";
    children = [
      V "x";
      C {
        node = Symbol "B";
        children = [V "y"; V "z"]
      }
    ]
  }




type substitution = (string * term) list


let rec var_exists var substitution =
  match substitution with
  | [] -> false
  | (s, _) :: rest -> s = var || var_exists var rest

let rec sigma_help signature substitution =
  match substitution with
  | [] -> true
  | (s, t) :: rest ->
      (wfterm signature t) && not (var_exists s rest) && (sigma_help signature rest)

let check_subst signature substitution=
  sigma_help signature substitution


let rec find subst var = match subst with
  | [] -> V var
  | (s,t) :: rest -> if s=var then t else find rest var



let sample_substitution = [
  ("x", V "leaf1");
  ("y", C { node = Symbol "A"; children = [V "leaf2"; V "leaf3"] });
  ("z", V "leaf4")
]


let rec subst sigma t =
  match t with
  | V x -> find sigma x
  | Int i -> Int i 
  | C r -> C {
    node = r.node;
    children = List.map (fun child -> subst sigma child) r.children
  }
;;

(* let rec subst_atom (s:substitution) (A(s', l): atom): atom = A(s', map (subst s) l)
;; *)
let rec subst_atom s a = 
  match a with 
  | A(symb,ls) -> A(symb,List.map (fun t -> subst s t) ls)

(* let compose_subst s1 s2 t = subst s2 (subst s1 t);; *)

let compose_subst s1 s2 =
  let result =
    List.fold_left
      (fun acc (x, t) ->
        if List.mem_assoc x s1 then
          acc
        else
          (x, t) :: acc)
      [] s2
  in
  List.map (fun (x, t) -> (x, subst s2 t)) s1 @ result



let rec mgu t u =
  match t, u with
  | Int i1 , Int i2 when i1=i2 -> []
  | Int _ , Int _ -> raise Ununifiable
  | V x, V y when x = y -> []  
  | V x, _ ->
    let u_vars = vars u in
    if StringSet.mem x u_vars then
      raise Ununifiable
    else
      [(x, u)]
  | _, V y ->
    let t_vars = vars t in
    if StringSet.mem y t_vars then
      raise Ununifiable
    else
      [(y, t)]

  | C { node = n1; children = c1 }, C { node = n2; children = c2 } ->
    if n1 <> n2 || List.length c1 <> List.length c2 then
      raise Ununifiable 
    else
      let rec unify_children sub children1 children2 =
        match children1, children2 with
        | [], [] -> sub  
        | t1 :: rest1, t2 :: rest2 ->
          let subst' = mgu (subst sub t1) (subst sub t2) in
          unify_children (compose_subst sub subst') rest1 rest2
        | _ -> 
          raise Ununifiable
      in
      unify_children [] c1 c2
  | _ -> raise Ununifiable

let subst1 = [("x", V "a"); ("y", V "b")]
let subst2 = [("z", V "c"); ("y", V "d")]
let result = compose_subst subst1 subst2
(* Expected result: [("x", V "a"); ("y", V "b"); ("z", V "c")] *)

(* Test the mgu function *)

(* let t1 = C { node = Symbol "f"; children = [V "x"; V "y"] }
let t2 = C { node = Symbol "f"; children = [V "z"; C { node = Symbol "g"; children = [V "z";V "f"] }] } *)

let a=C { node = (Symbol "*"); children = [ C{node =(Symbol "+");children=[V "x"; V "y"]}; V "x"]};;
let b=C { node = (Symbol "*"); children = [C{node =(Symbol "+");children=[V "a"; V "b"]};C{node =(Symbol "-");children=[ V "b"]} ]};;
let mgu_result = mgu a b 
(* Expected result: [("x", V "z"); ("y", C { node = Symbol "g"; children = [V "z"] })] *)

(* Print the results *)
(* let print_subst subst =
  List.iter (fun (x, t) -> Printf.printf "(%s, %s) " x (match t with V s -> s | C { node = n; children = c } -> Printf.sprintf "C[%s; %s]" (match n with Symbol s -> s) (String.concat " " (List.map (function V s -> s | _ -> "C") c)))) subst;
  print_newline () *)

  let print_subst subst =
    List.iter (fun (x, t) -> Printf.printf "(%s, " x; print_term t; print_string ") ") subst;
    print_newline ()
;;



let sig1= [(Symbol "0", 0); (Symbol "1", 0); (Symbol "0", 1)];;
let sig2 = [(Symbol "0", 0); (Symbol "1", 0); (Symbol "+", 2)];;

let t = C {node = (Symbol "+"); children = [(V "x"); (V "y"); (V "z")]} ;;
let t2 = C {node = (Symbol "+"); children = [(V "x"); (V "y")]} ;;
let t3 = C {node = (Symbol "+"); children = [(V "z"); t2]} ;;
    
  

let () =
  if check_sig sig1 then
    print_endline "Signature is valid."
  else
    print_endline "Signature is not valid." 
  ;
  if (wfterm sig2 t) then
    print_endline "term is well-formed according to the signature."
  else
    print_endline "term is not well-formed according to the signature."
  ;
  (* print_subst result; *)
  print_subst mgu_result
;;





let rec modifyTerm i t= match t with
  | Int _ -> t
  | V v -> V ((string_of_int i) ^ v)
  | C {node; children} -> C {node; children = List.map (modifyTerm i) children}
;;

let rec modifyAtom i a= match a with
  A(s, l) -> A(s, map (modifyTerm i) l)
;;

let rec modifyClause cl i = match cl with
    Fact(H(a)) -> Fact(H(modifyAtom i a))
  | Rule(H(a), B(l)) -> Rule(H(modifyAtom i a), B(map (modifyAtom i) l))
;;

let rec modifyInitialProg prog i= match prog with
    [] -> []
  | cl::ps -> (modifyClause cl i)::modifyInitialProg ps (i+1)
;;

let rec modifyProg2 (prog:program) (A(s, _): atom): program = match prog with
    [] -> []
  | cl::ps -> match cl with Fact(H(A(s', _))) | Rule(H(A(s', _)), _) ->
                if s = s' then (modifyClause cl 0)::modifyProg2 ps (A(s, []))
                else cl::modifyProg2 ps (A(s, []))
;;



let rec print_term_list (tl:term list) = match tl with
    [] -> Printf.printf ""
  | [t] -> print_term t
  | t::tls -> (
      print_term t;
      Printf.printf ",";
      print_term_list tls;
    )

and print_term (t:term) = match t with
  | Int i -> Printf.printf "%d" i
  | V(v) -> Printf.printf " %s " v
  | C {node = Symbol n; children} ->
    Printf.printf "%s" n;
    if children <> [] then (
      Printf.printf "(";
      print_term_list children;
      Printf.printf ")"
    )
;;




type variable=string  

let rec getSolution (unif:substitution) (vars:variable list) = match vars with
    [] -> []
  | v::vs ->
      let rec occurs l = match l with
          [] -> raise NotFound
        | x::xs -> if (fst x) = v then x
                    else occurs xs
      in
      try (occurs unif)::getSolution unif vs
      with NotFound -> getSolution unif vs
;;

let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
          { termio with Unix.c_icanon = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res


let rec printSolution (unif:substitution) = match unif with
    [] -> Printf.printf "true. "
  | [(v, t)] -> (
      Printf.printf "%s =" v;
      print_term t;
    )
  | (v, t)::xs -> (
      Printf.printf "%s =" v;
      print_term t;
      Printf.printf ", ";
      printSolution xs;
    )
;;

let mgu_atom (A(s1, l1): atom) (A(s2, l2): atom): substitution = mgu (C{node=s1;children=l1}) (C{node=s2;children=l2})
;;

let solve_atom_atom (a1:atom) (a2:atom) (unif:substitution): substitution =
  compose_subst unif (mgu_atom (subst_atom unif a1) (subst_atom unif a2))
;;

let solve_term_term (t1:term) (t2:term) (unif:substitution): substitution =
  compose_subst unif (mgu (subst unif t1) (subst unif t2))
;;


let rec apply_arithmetic (t: term): term =
  match t with
  | Int _ -> t
  | C { node = Symbol "+"; children = [t1; t2] } ->
    let simplified_t1 = apply_arithmetic t1 in
    let simplified_t2 = apply_arithmetic t2 in
    (match (simplified_t1, simplified_t2) with
    | (Int i1, Int i2) -> Int (i1 + i2) 
    | _ -> raise Ununifiable)  
  | C { node = Symbol "*"; children = [t1; t2] } ->
    let simplified_t1 = apply_arithmetic t1 in
    let simplified_t2 = apply_arithmetic t2 in
    (match (simplified_t1, simplified_t2) with
    | (Int i1, Int i2) -> Int (i1 * i2)  
    | _ -> raise Ununifiable )
  | C { node = Symbol "-"; children = [t1; t2] } ->
    let simplified_t1 = apply_arithmetic t1 in
    let simplified_t2 = apply_arithmetic t2 in
    (match (simplified_t1, simplified_t2) with
    | (Int i1, Int i2) -> Int (i1 - i2)  
    | _ -> raise Ununifiable)
  | C { node = Symbol "/"; children = [t1; t2] } ->
    let simplified_t1 = apply_arithmetic t1 in
    let simplified_t2 = apply_arithmetic t2 in
    (match (simplified_t1, simplified_t2) with
    | (Int i1, Int i2) -> Int (i1 / i2)  
    | _ -> raise Ununifiable)
  | _ -> t 
;;


let atomic_operators (a: atom) (unif: substitution): bool*substitution =
  match a with
  | A(Symbol "=", [t1; t2]) ->
    print_endline "Debugging: Before calculating simplified_t1";
    let simplified_t1 = apply_arithmetic (subst unif t1) in
    let simplified_t2 = apply_arithmetic (subst unif t2) in
    (match (simplified_t1, simplified_t2) with
    | (Int i1, Int i2) -> (i1 = i2 , unif) 
    | _ -> raise Ununifiable)    
  | A(Symbol "!=", [t1; t2]) ->
    let simplified_t1 = apply_arithmetic (subst unif t1) in
    let simplified_t2 = apply_arithmetic (subst unif t2) in
    (match (simplified_t1, simplified_t2) with
    | (Int i1, Int i2) -> (i1 <> i2 , unif) 
    | _ -> raise Ununifiable)   
  | A(Symbol "<", [t1; t2]) ->
    let simplified_t1 = apply_arithmetic (subst unif t1) in
    let simplified_t2 = apply_arithmetic (subst unif t2) in
    (match (simplified_t1, simplified_t2) with
    | (Int i1, Int i2) -> (i1 < i2, unif)  
    | _ -> raise Ununifiable)    
  | A(Symbol "<=", [t1; t2]) ->
    let simplified_t1 = apply_arithmetic (subst unif t1) in
    let simplified_t2 = apply_arithmetic (subst unif t2) in
    (match (simplified_t1, simplified_t2) with
    | (Int i1, Int i2) -> (i1 <= i2  , unif)
    | _ -> raise Ununifiable)    
  | A(Symbol ">", [t1; t2]) ->
    print_endline "Debugging: Before calculating simplified_t1";
    let simplified_t1 = (apply_arithmetic (subst unif t1)) in
    let simplified_t2 = (apply_arithmetic (subst unif t2)) in
    (match (simplified_t1, simplified_t2) with
    | (Int i1, Int i2) -> (i1 > i2  , unif)
    | _ -> raise Ununifiable)  
  | A(Symbol ">=", [t1; t2]) ->
    let simplified_t1 = apply_arithmetic (subst unif t1) in
    let simplified_t2 = apply_arithmetic (subst unif t2) in
    (match (simplified_t1, simplified_t2) with
    | (Int i1, Int i2) -> (i1 >= i2  , unif)
    | _ -> raise Ununifiable)  
  | A( Symbol "IS", [t1;t2]) ->
    print_endline "Debugging: Before calculating simplified_t1";
    let simplified_t1 = apply_arithmetic (subst unif t1) in
    let simplified_t2 = apply_arithmetic (subst unif t2) in
    (true,compose_subst unif (mgu simplified_t1 simplified_t2))
  | _ -> raise Ununifiable      


let rec solve_goal (prog:program) (g:goal) (unif:substitution) (vars:variable list): (bool * substitution) =
  match g with
    | G([]) -> (
        printSolution (getSolution unif vars);
        flush stdout;
        let choice = ref (get1char()) in
        while(!choice <> '.' && !choice <> ';') do
          Printf.printf "\nUnknown Action: %c \nAction? " (!choice);
          flush stdout;
          choice := get1char();
        done;
        Printf.printf "\n";
        if !choice = '.' then (true, [])
        else (false, [])
      )
    | G(a::gs) -> match a with  
        | A (Symbol "=", [t1; t2]) | A (Symbol "!=", [t1; t2]) | A (Symbol "<", [t1; t2]) | A (Symbol "<=", [t1; t2])
        | A (Symbol ">", [t1; t2]) | A (Symbol ">=", [t1; t2]) | A (Symbol "IS", [t1; t2])->
          (try
            let (result, s') = atomic_operators a unif in
            if result then solve_goal prog (G gs) s' vars
            else (false, [])
          with Ununifiable -> (false, []))
        | _ ->
          let new_prog = modifyProg2 prog a in
          let rec iter prog' = match prog' with
            |  [] -> (false, [])
            | cl::ps -> match cl with
              | Fact(H(a')) -> (
                  try
                    let u = (solve_atom_atom a' a unif) in
                    match (solve_goal new_prog (G(gs)) u vars) with
                      |(true, u') -> (true, u')
                      | _ -> iter ps
                  with Ununifiable -> iter ps
                )
              | Rule(H(a'), B(al)) -> (
                  try
                    let u = (solve_atom_atom a' a unif) in
                    match (solve_goal new_prog (G(al @ gs)) u vars) with
                      | (true, u') -> (true, u')
                      | _ -> iter ps
                  with Ununifiable -> iter ps
                )
        in iter prog
;;
let rec vars_term (t:term): string list =
  match t with
  | Int _ -> []
  | V v -> [v]
  | C {node = _; children} ->
      List.fold_left (fun acc child -> acc @ vars_term child) [] children
;;

let vars_atom (A(s, l): atom): variable list = vars_term (C{node=s;children=l})
;;

let rec vars_goal (G atoms) =
  List.fold_left (fun acc atom -> acc @ vars_atom atom) [] atoms

let interpret_goal (prog:program) (g:goal) = solve_goal prog g [] (vars_goal g)
;;



(* let rec solve_atom_program a p g s v =
  match a with  
    | A (Symbol "EQ", [t1; t2]) | A (Symbol "NEQ", [t1; t2]) | A (Symbol "L", [t1; t2]) | A (Symbol "LE", [t1; t2])
    | A (Symbol "G", [t1; t2]) | A (Symbol "GE", [t1; t2]) | A (Symbol "IS", [t1; t2])->
    (try
      let (result, s') = atomic_operators a unif in
      if result then solve_goal prog (G gs) s' vars
      else (false, [])
    with Ununifiable -> (false, []))
    | _ ->
      match p with
        |  [] -> (false, [])
        | cl::ps -> match cl with
          | Fact(H(a')) -> 
            (try
              let u = (solve_atom_atom a' a unif) in
              match (solve_goal new_prog (G(gs)) u vars) with
                |(true, u') -> (true, u')
                | _ -> iter ps
            with Ununifiable -> iter ps
            )
          | Rule(H(a'), B(al)) -> (
            try
              let u = (solve_atom_atom a' a unif) in
              match (solve_goal new_prog (G(al @ gs)) u vars) with
                | (true, u') -> (true, u')
                | _ -> iter ps
            with Ununifiable -> iter ps
          )
  in iter prog   




let rec solve_goal_2 p g s v =
  match g with
    | G([]) -> (
        printSolution (getSolution unif vars);
        flush stdout;
        let choice = ref (get1char()) in
        while(!choice <> '.' && !choice <> ';') do
          Printf.printf "\nUnknown Action: %c \nAction? " (!choice);
          flush stdout;
          choice := get1char();
        done;
        Printf.printf "\n";
        if !choice = '.' then (true, [])
        else (false, [])
      )
    | G(a::gs) -> match a with  
        | A (Symbol "EQ", [t1; t2]) | A (Symbol "NEQ", [t1; t2]) | A (Symbol "L", [t1; t2]) | A (Symbol "LE", [t1; t2])
        | A (Symbol "G", [t1; t2]) | A (Symbol "GE", [t1; t2]) | A (Symbol "IS", [t1; t2])->
          (try
            let (result, s') = atomic_operators a unif in
            if result then solve_goal prog (G gs) s' vars
            else (false, [])
          with Ununifiable -> (false, []))
        | _ ->
          let new_prog = modifyProg2 prog a in
          let rec iter prog' = match prog' with
            |  [] -> (false, [])
            | cl::ps -> match cl with
              | Fact(H(a')) -> (
                  try
                    let u = (solve_atom_atom a' a unif) in
                    match (solve_goal new_prog (G(gs)) u vars) with
                      |(true, u') -> (true, u')
                      | _ -> iter ps
                  with Ununifiable -> iter ps
                )
              | Rule(H(a'), B(al)) -> (
                  try
                    let u = (solve_atom_atom a' a unif) in
                    match (solve_goal new_prog (G(al @ gs)) u vars) with
                      | (true, u') -> (true, u')
                      | _ -> iter ps
                  with Ununifiable -> iter ps
                )
        in iter prog
;; *)