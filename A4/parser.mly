%{
    open Ast
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
%token EOF
%token Bool_true
%token Bool_false
%token AND
%token OR
%token NOT

%left "+" "-"
%left "*" "/"
%nonassoc UMINUS


%start main
%type <Ast.expr_list> main


%%
main:
    | clauses EOF
        { ExprList($1) }
clauses:
    | clause DOT
        { [$1] }
    | clause DOT clauses
        { [$1] @ $3 } 
    
clause:
    | atom
        { Fact($1) }
    | atom EQUAL goal
        { Rule($1,Goal($3)) }
    // this states that input of form expr followed by eof and $1 means return first terminal/ non terminal
goal:
    | atom             { [$1] }    
    | goal COMMA goal   { $1 @ $3 }  
    | LPAREN goal RPAREN { $2 }  

atom:
    | IDENT LPAREN arglist RPAREN
        { Func(Fname($1),Arguments($3)) }
    // | term ADD term
    //     { BinOp(BopAdd,$1,$3)}
    // | term MUL term
    //     { BinOp(BopMul,$1,$3)}
    // | term SUB term
    //     { BinOp(BopSub,$1,$3)}
    // | term DIV term
    //     { BinOp(BopDiv,$1,$3)}
    | Bool_true
        { Boolconst(Booltrue) }
    | Bool_false
        { Boolconst(Boolfalse) }

arglist:
    | term             { [$1] }    
    | arglist COMMA arglist   { $1 @ $3 }  
    | LPAREN term RPAREN { [$2] } 

term:
    | IDENT LPAREN arglist RPAREN
        { Func(Fname($1),Arguments($3)) }
    | INT 
        { Int($1) }
    | Bool_true
        { Boolconst(Booltrue) }
    | Bool_false
        { Boolconst(Boolfalse) }
    | IDENT
        { Var($1) }
    | term ADD term
        { BinOp(BopAdd,$1,$3)}
    | term MUL term
        { BinOp(BopMul,$1,$3)}
    | term SUB term
        { BinOp(BopSub,$1,$3)}
    | term DIV term
        { BinOp(BopDiv,$1,$3)}
    | term AND term 
        { Boolop(AND,$1,$3) }
    | term OR term 
        { Boolop(OR,$1,$3) }
    | NOT term 
        { UnOp(NOT,$2) }
    | SUB term 
        { UnOp(UnopMinus,$2) }
    | LPAREN term RPAREN
        { $2 }

