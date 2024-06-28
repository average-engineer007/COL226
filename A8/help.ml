type tree = V of string | Num of int | C of {node: symbol; children: tree list};; 
type term = V of variable | Num of int | Node of symbol * (term list)
type atom = A of symbol * (term list)
type head = H of atom
type body = B of atom list
type clause = F of head | R of head * body
type program = clause list
type goal = G of atom list
type substitution = (variable * term) list
type symbol = Symbol of string
type tree = V of string
| C of {node: symbol; children: tree list};; 