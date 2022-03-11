/* Ocamlyacc parser for GoSci */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE COLON DOT
%token PLUS MINUS MUL DIV MOD POW MATMUL INC DEC ASSIGN IASSIGN
%token EQ NEQ LT NOT AND OR
%token VAR CONST STRUCT UNIT VARIANT IF ELSE SWITCH MATCH CASE WHILE FOR CONTINUE BREAK FUNC
%token BOOL INT FLOAT CHAR STRING TENSOR
%token RETURN COMMA
%token <bool> BLIT
%token <int> ILIT
%token <float> FLIT
%token <string> CLIT
%token <string> SLIT
%token <string> ID
%token EOF

/* For parsing */
%start program_rule
%type <Ast.program> program_rule
/* For scanning only */
%start tokenseq
%type <Ast.tokenseq> tokenseq

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS

%%

program_rule:
  vdecl_list_rule stmt_list_rule EOF { {locals=$1; body=$2} }

vdecl_list_rule:
  /*nothing*/                   { []       }
  | vdecl_rule vdecl_list_rule  { $1 :: $2 }

vdecl_rule:
  typ_rule ID SEMI { ($1, $2) }


typ_rule:
  INT       { Int  }
  | BOOL    { Bool }

stmt_list_rule:
    /* nothing */               { []     }
    | stmt_rule stmt_list_rule  { $1::$2 }

stmt_rule:
  expr_rule SEMI                                          { Expr $1         }
  | LBRACE stmt_list_rule RBRACE                          { Block $2        }
  | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule   { If ($3, $5, $7) }
  | WHILE LPAREN expr_rule RPAREN stmt_rule               { While ($3,$5)   }

expr_rule:
  | BLIT                          { BoolLit $1            }
  | ILIT                          { ILIT $1               }
  | ID                            { Id $1                 }
  | expr_rule PLUS expr_rule      { Binop ($1, Add, $3)   }
  | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3)   }
  | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
  | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3)   }
  | expr_rule LT expr_rule        { Binop ($1, Less, $3)  }
  | expr_rule AND expr_rule       { Binop ($1, And, $3)   }
  | expr_rule OR expr_rule        { Binop ($1, Or, $3)    }
  | ID ASSIGN expr_rule           { Assign ($1, $3)       }
  | LPAREN expr_rule RPAREN       { $2                    }


/* Scan only
   For testing
 */
tokenseq:
  tokens EOF { $1 }

tokens:
   /* nothing */ { [] }
 | one_token tokens { $1 :: $2 }

one_token:
  | LPAREN { "(" }
  | RPAREN { ")" }
  | LBRACK { "[" }
  | RBRACK { "]" }
  | LBRACE { "{" }
  | RBRACE { "}" }
  | COLON  { ":" }
  | SEMI  {  ";" }
  | COMMA { "," }
  | DOT   { "." }
  /* Operators */
  | PLUS { "PLUS" }
  | MINUS { "MINUS" }
  | MUL { "MUL" }
  | DIV { "DIV" }
  | MOD { "MOD" }
  | POW { "POW" }
  | MATMUL { "MATMUL" }
  | INC { "INC" }
  | DEC { "DEC" }
  | ASSIGN { "ASSIGN" }
  | IASSIGN { "IASSIGN" }
  | EQ { "EQ" }
  | NEQ { "NEQ" }
  | LT { "LT" }
  | NOT { "NOT" }
  | AND { "AND" }
  | OR { "OR" }
  /* Keywords */
  | VAR { "VAR" }
  | CONST { "CONST" }
  | STRUCT { "STRUCT" }
  | UNIT {"UNIT"}
  | VARIANT {"VARIANT"}
  | IF { "IF" }
  | ELSE { "ELSE" }
  | SWITCH {"SWITCH"}
  | MATCH {"MATCH"}
  | CASE {"CASE"}
  | WHILE { "WHILE" }
  | FOR {"FOR"}
  | CONTINUE {"CONTINUE"}
  | BREAK {"BREAK"}
  | FUNC {"FUNC"}
  | RETURN { "RETURN" }
  /* Types */
  | BOOL { "BOOL" }
  | INT { "INT" }
  | FLOAT {"FLOAT"}
  | CHAR {"CHAR"}
  | STRING {"STRING"}
  | TENSOR {"TENSOR"}
  /* Literals */
  | BLIT { string_of_bool $1 ^ ":BOOL" }
  | ILIT { string_of_int $1 ^ ":INT" }
  | FLIT { string_of_float $1 ^ ":FLOAT" }
  | CLIT { $1 ^ ":CHAR" }
  | SLIT { $1 ^ ":STRING" }
  | ID { $1 ^ ":ID" }
