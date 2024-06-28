{
    open Token
}
(* it is a tokeniser not a parser still we write parse ironic *)
rule token = parse
    [' ' '\t' '\n']
        { token lexbuf }
        
    | "if"
        { KEYWORD "IF" }
    | "else"
        { KEYWORD "ELSE" }
    | "elif"
        { KEYWORD "ELSE IF"}
    | "then"
        { KEYWORD "THEN" }
    | "and"
        { KEYWORD "AND" }
    | "or"
        { KEYWORD "OR" }
    | "not"
        { KEYWORD "NOT" }

    | "+"
        { ARITH_OP "ADD" }
    | "*"
        { ARITH_OP "MUL" }
    | "-"     
        { ARITH_OP "MINUS" }
    | "%"
        { ARITH_OP "MOD" }
    | "/"           
        { ARITH_OP "DIV" }
    | ['0'-'9']+ as lxm
        { INT(int_of_string lxm) }

    | '_' ['a'-'z' 'A'-'Z' '0'-'9' '_' ''']* as id
        { IDENTIFIER id }
    | ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ''']* as id
        { IDENTIFIER id }

    | "true" 
        { BOOL_CONST true }
    | "false" 
        { BOOL_CONST false }
    | "&&" 
        { BOOL_OP "AND" }
    | "||" 
        { BOOL_OP "OR" }
    | "~" 
        { BOOL_OP "NOT" }

    | "==" 
        { COMPARISON_OP "EQ" }
    | "!=" 
        { COMPARISON_OP "NEQ" }
    | "<" 
        { COMPARISON_OP "LT" }
    | ">" 
        { COMPARISON_OP "GT" }
    | "<=" 
        { COMPARISON_OP "LTE" }
    | ">=" 
        { COMPARISON_OP "GTE" }
    | "="
        { ASSIGNMENT_OP }

    | '\"' ['a'-'z' 'A'-'Z' '0'-'9']+ "\"" as scons
        { STRING_CONST scons}
    | "^"
        { STRING_OP "CONCATENATE" }

    | ','
        { COMMA }
    | '('          
        { LPAREN }
    | ')'            
        { RPAREN }

    | ['!' '$' '&' '.' ':' '?' '@' '\\' '^' '`' '|' '~' '0'-'9' 'A'-'Z'] ['!' '$' '&' '.' '/' ':' '?' '@' '\\' '`' '|' '~' 'a'-'z' 'A'-'Z' '0'-'9' '_']* as er
        { ERRORT er }
    | _ as error
      { ERROR (Lexing.lexeme lexbuf) }
    | eof
        { EOF }
