type tok=
  | KEYWORD of string
  | ARITH_OP of string
  | INT of int
  | IDENTIFIER of string
  | BOOL_CONST of bool
  | BOOL_OP of string
  | COMPARISON_OP of string
  | ASSIGNMENT_OP
  | STRING_CONST of string
  | STRING_OP of string
  | COMMA
  | LPAREN
  | RPAREN  
  | ERRORT of string
  | ERROR of string
  | EOF

