%{
    open Interpreter
%}


%token <int> INT
%token ADD 
%token MUL 
%token SUB 
%token DIV 
%token LPAREN 
%token RPAREN 
%token LET
%token IN
%token EQUAL
%token DOT
%token COMMA
%token <string> IDENT
%token <string> VAR
%token EOF
%token Bool_true
%token Bool_false
%token AND
%token OR
%token NOT
%token COND
%token IS
%token EQ
%token NEQ
%token G
%token GE
%token L
%token LE


// %left "+" "-"
// %left "*" "/"
// %nonassoc UMINUS


%start main goal
%type <Interpreter.program> main
%type <Interpreter.goal> goal


%%
main:
    | clauses 
        { ($1) }
clauses:
    | clause DOT
        { [$1] }
    | clause DOT clauses
        { [$1] @ $3 } 
    
clause:
    | atom
        { Fact(H($1)) }
    | atom COND atom_list
        { Rule(H($1),B($3)) }
    // this states that input of form expr followed by eof and $1 means return first terminal/ non terminal

atom_list:
    | atom
        { [$1] }
    | atom COMMA atom_list                
        { [$1] @ $3 }

goal:
    | atom_list DOT
        { G($1) }

atom:
    | IDENT LPAREN arglist RPAREN  
        { A(Symbol ($1),$3) }
    | term EQ term                        
        { A(Symbol ("=") , [$1; $3]) } 
    | term NEQ term                    
        { A(Symbol ("!=") , [$1; $3]) } 
    | term L term                        
        { A(Symbol ("<") , [$1; $3]) }
    | term LE term                        
        { A(Symbol ("<=") , [$1; $3]) } 
    | term G term                        
        { A(Symbol (">") , [$1; $3]) }
    | term GE term                        
        { A(Symbol (">=") , [$1; $3]) }
    | term IS term                        
        { A(Symbol ("IS") , [$1; $3]) }

    // | term 
    // | term ADD term
    //     { BinOp(BopAdd,$1,$3)}
    // | term MUL term
    //     { BinOp(BopMul,$1,$3)}
    // | term SUB term
    //     { BinOp(BopSub,$1,$3)}
    // | term DIV term
    //     { BinOp(BopDiv,$1,$3)}
    // | Bool_true
    //     { Boolconst(Booltrue) }
    // | Bool_false
    //     { Boolconst(Boolfalse) }

arglist:
    | term             { [$1] }    
    | arglist COMMA arglist  { $1 @ $3 }  
    | LPAREN term RPAREN { [$2] } 

term:
    | IDENT LPAREN arglist RPAREN  
        { C({node = Symbol($1); children = $3}) }
    | IDENT
        { C({node = Symbol($1); children = []}) }
    | INT 
        { Int($1) }
    // | Bool_true
    //     { Boolconst(Booltrue) }
    // | Bool_false
    //     { Boolconst(Boolfalse) }
    | VAR
        { V($1) }
    | term ADD term
        { C({node = Symbol("+"); children = [$1; $3]}) }
    | term MUL term
        { C({node = Symbol("*"); children = [$1; $3]}) }
    | term SUB term
        { C({node = Symbol("-"); children = [$1; $3]}) }
    | term DIV term
        { C({node = Symbol("/"); children = [$1; $3]}) }
    // | term AND term 
    //     { Boolop(AND,$1,$3) }
    // | term OR term 
    //     { Boolop(OR,$1,$3) }
    // | NOT term 
    //     { UnOp(NOT,$2) }
    // | SUB term 
    //     { UnOp(UnopMinus,$2) }
    | LPAREN term RPAREN
        { $2 }

