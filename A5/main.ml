module StringSet = Set.Make(String);;
exception Ununifiable of string;;


type symbol = Symbol of string
type tree = V of string
| C of {node: symbol; children: tree list};; 




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

(* let () =
  if check_sig sample_signature then
    print_endline "Signature is valid."
  else
    print_endline "Signature is not valid." *)

(* let rec count_children symbol t = 
  match t with
  | V _ -> 0
  | C { node = n; children = children } -> List.length children  *)


(*     
let rec wftree signature t = 
  match t with 
    | V _ -> true
    | C { node=n ; children=children} -> List.length children *)


  
let rec find_arity symbol signature =
  match signature with
    | [] -> -1
    | (s, a) :: rest -> if s=symbol then a else find_arity symbol rest


let rec helper signature t =
  match t with
  | V _ -> true
  | C { node = n; children = children } ->
    if find_arity n signature = List.length children then
      List.fold_left (fun acc child -> acc && helper signature child) true children
    else
      false

let wftree signature t =
  check_sig signature && helper signature t 
  
(* let rec wftree signature t =
  let rec count_children symbol t = 
    match t with
    | V _ -> 0
    | C { node = n; children = children } -> if n = symbol then List.length children 
      List.fold_left (fun acc x -> acc + count_children symbol x) 0 children
  in
  let rec helper sigs t =
    match (sigs, t) with
    | [], V _ -> true
    | (Symbol s, n) :: tl, C { node = nn; children = children } ->
      if s = nn && n = List.length children then
        List.for_all (fun (s, c) -> count_children s t = c) tl && List.for_all (helper tl) children
      else
        false
    | _ -> false
  in
  check_sig signature && helper signature t *)

(* let () =
let sample_symbol = Symbol "C" in
let arity = find_arity sample_symbol sample_signature in
if arity >= 0 then
  Printf.printf "Arity of symbol is %d\n"  arity
else
  Printf.printf "Symbol not found in the signature\n" *)

let sample_tree_valid =
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

let sample_tree_invalid =
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
  V _ -> 0
  | C {node;children} ->
    if List.length children = 0 then
      1 
    else
      1 + List.fold_left (fun acc child -> max acc (ht child)) 0 children

let rec size t = match t with
  V _ -> 1
  | C { children = children } ->
    1 + List.fold_left (fun acc child -> acc + size child) 0 children




let rec vars t =
  match t with
  | V v -> StringSet.singleton v  
  | C { children = children } ->

    List.fold_left (fun acc child -> StringSet.union acc (vars child)) StringSet.empty children

let print_set set =
  let elements_list = StringSet.elements set in
  List.iter (fun x -> print_string x; print_string " ") elements_list;
  print_newline ()
(* let () =
  let variables_set = vars sample_tree_valid in
  print_set variables_set *)

  (* if (wftree sample_signature sample_tree_valid) then
    print_endline "Sample tree is well-formed according to the signature."
  else
    print_endline "Sample tree is not well-formed according to the signature.";

  if (wftree sample_signature sample_tree_invalid) then
    print_endline "Sample tree is well-formed according to the signature."
  else
    print_endline "Sample tree is not well-formed according to the signature."; *)



let rec mirror_tree t =
  match t with
  | V _ -> t  
  | C { node = n; children = children } ->
    let mirrored_children = List.map mirror_tree (List.rev children) in
    C { node = n; children = mirrored_children }





let rec print_tree t =
  match t with
  | V v -> print_string v 
  | C { node = n; children = children } ->
    print_string (match n with Symbol s -> s);
    print_string " [";
    List.iter (fun child -> print_tree child; print_string ", ") children;
    print_string "]"

let sample_tree =
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



(* let () =
print_tree sample_tree;
  print_newline ();
  let mirrored_tree = mirror_tree sample_tree in
  print_tree mirrored_tree;
  print_newline () *)


type substitution = (string * tree) list


let rec var_exists var substitution =
  match substitution with
  | [] -> false
  | (s, _) :: rest -> s = var || var_exists var rest

let rec sigma_help signature substitution =
  match substitution with
  | [] -> true
  | (s, t) :: rest ->
      (wftree signature t) && not (var_exists s rest) && (sigma_help signature rest)

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

(* let () =
  (* Test cases *)
  let result1 = find sample_substitution "x" in
  Printf.printf "Substitution for 'x': %s\n" (match result1 with V s -> s | _ -> "Invalid");

  let result2 = find sample_substitution "y" in
  Printf.printf "Substitution for 'y':\n"; print_tree result2;

  let result3 = find sample_substitution "z" in
  Printf.printf "Substitution for 'z': %s\n" (match result3 with V s -> s | _ -> "Invalid");

  let result4 = find sample_substitution "w" in
  Printf.printf "Substitution for 'w': %s\n" (match result4 with V s -> s | _ -> "Invalid") *)




let rec subst sigma t =
  match t with
  | V x -> find sigma x
  | C r -> C {
    node = r.node;
    children = List.map (fun child -> subst sigma child) r.children
  }
;;

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

(* let compose_subst subst1 subst2 =
  subst2 @ (List.map (fun (v, t) -> (v, subst2 |> List.assoc_opt t |> Option.value t)) subst1) *)

let rec mgu t u =
  match t, u with
  | V x, V y when x = y -> []  
  | V x, _ ->
    let u_vars = vars u in
    if StringSet.mem x u_vars then
      raise (Ununifiable "Variable is in the set of variables of u")
    else
      [(x, u)]
  | _, V y ->
    let t_vars = vars t in
    if StringSet.mem y t_vars then
      raise (Ununifiable "Variable is in the set of variables of u")
    else
      [(y, t)]

  | C { node = n1; children = c1 }, C { node = n2; children = c2 } ->
    if n1 <> n2 || List.length c1 <> List.length c2 then
      raise (Ununifiable "Nodes are different or children lists have different lengths") 
    else
      let rec unify_children sub children1 children2 =
        match children1, children2 with
        | [], [] -> sub  
        | t1 :: rest1, t2 :: rest2 ->
          let subst' = mgu (subst sub t1) (subst sub t2) in
          unify_children (compose_subst sub subst') rest1 rest2
        | _ -> 
          raise (Ununifiable "Nodes are different or children lists have different lengths")
      in
      unify_children [] c1 c2

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
    List.iter (fun (x, t) -> Printf.printf "(%s, " x; print_tree t; print_string ") ") subst;
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
  if (wftree sig2 t) then
    print_endline "tree is well-formed according to the signature."
  else
    print_endline "tree is not well-formed according to the signature."
  ;
  (* print_subst result; *)
  print_subst mgu_result
let () =
  (* print_tree t3; *)
  print_newline ();
  let mirrored_tree = mirror_tree t3 in
  print_tree mirrored_tree;
  print_newline ();
  let tree_size = size t2 in
  Printf.printf "Size of the tree: %d\n" tree_size;
  let height = ht t2 in
  Printf.printf "Height of the tree: %d\n" height
;;




  (* 1
  let a=C{node=(Symbol "A");children=[]};;
  let b=C{node=(Symbol "B");children=[]};;
  
  
  (* 2 *)
  let a=C{node=(Symbol "A");children=[]};;
  let b=C{node=(Symbol "A");children=[]};;
  
  
  (* 3 *)
  let a=V y;;
  let b=V x;;
  
  
  (* 4 *)
  let a=V x;;
  let b=V y;;
   *)
(*   
  5
  let a=C{node=(Symbol "A");children=[V x ; C{node=(Symbol "B");children=[V f, V g]}]};;
  let b=C{node=(Symbol "A");children=[C{node=(Symbol "L");children=[V x;V y]};C{node=(Symbol "B");children=[V f, V g]}]};;

  6 
  let a=C{node=(Symbol "A");children=[V x ; V x ]};;
  let b=C{node=(Symbol "A");children=[V y ; V z]};;

  7
  let a=C{node=(Symbol "A"); children = [V "x";C { node = (Symbol "B"); children = [V "e"; V "c"] }]};;
  let b=C{node=(Symbol "A"); children = [V "y";C { node = (Symbol "B"); children = [V "g"; V "k"] }]};;
  
  8
  let a=C{node=(Symbol "A"); children = [c{node=(Symbol "T";children[V "f"; C{node = (Symbol "G";children=[V "d"])}])};C { node = (Symbol "B"); children = [V "e"; V "c"] }]};;
  let b=C{node=(Symbol "A"); children = [V "y";C { node = (Symbol "B"); children = [V "g"; V "k"] }]};; *)


  
  