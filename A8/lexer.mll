
{
  open Parser
  exception Error of char
}

let letter = ['A'-'Z'] | ['a'-'z']
let digit = ['0'-'9']
let non_digit = '_' | letter
let ident_start = ['a'-'z']
let ident = ident_start (digit | non_digit)*
let all=letter | '_' | digit
let var_name = ['A'- 'Z'] (all)*

let line_comment = "//" [^ '\n']*

rule token = parse
    |  [' ' '\t'] | line_comment
        { token lexbuf }
    | ['\n']
        { Lexing.new_line lexbuf; token lexbuf }
    | ident as s
        { match s with
          | "is" -> IS
          | s -> IDENT(s)
        }
    | var_name as s
        { VAR(s) }
    | "=:="
        { EQ }
    | "=\\="
        { NEQ }
    | "<"
        { L}
    | "<="
        { LE}
    | ">"
        { G }
    | ">="
        { GE }
    | ":-"
        { COND }
    | ['+']
        { ADD }
    | ['-']
        { SUB }
    | ['*']
        { MUL }
    | ['/']
        { DIV }


    | "&&" 
        { AND }
    | "||" 
        { OR }
    | "~" 
        { NOT }

    | ['.']
        { DOT }
    | [',']
        { COMMA }
    | digit+ as lxm
        { INT(int_of_string lxm) }
    | ['(']
        { LPAREN }
    | [')']
        { RPAREN }
    | eof
        { EOF }
    | _ as c
        { raise (Error c) }  