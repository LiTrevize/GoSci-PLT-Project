/* Ocamlyacc parser for GoSci */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE COLON DOT VERBAR
%token PLUS MINUS MUL DIV MOD POW MATMUL INC DEC ASSIGN IASSIGN
%token EQ NEQ LT NOT AND OR
%token VAR CONST STRUCT UNIT VARIANT IF ELSE SWITCH MATCH CASE WHILE FOR CONTINUE BREAK FUNC
%token BOOL INT FLOAT CHAR STRING TENSOR
%token RETURN COMMA
%token <bool> BLIT
%token <int> ILIT
%token <float> FLIT
%token <char> CLIT
%token <string> SLIT
%token <string> ID
%token EOF

/* For parsing */
%start program
%type <Ast.program> program
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

program:
  decls EOF { $1}

decls:
   /* nothing */    { ([], [], [])               }
 | vdecl SEMI decls { match $3 with (v, u, f) -> (($1 :: v), u, f) }
 | udecl decls      { match $2 with (v, u, f) -> (v, ($1 :: u), f) }
 | fdecl decls      { match $2 with (v, u, f) -> (v, u, ($1 :: f)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
  typ ID { ($1, $2) }

typ:
    INT    { Int    }
  | BOOL   { Bool   }
  | FLOAT  { Float  }
  | CHAR   { Char   }
  | STRING { Str }

ids_opt:
  /*nothing*/ { BaseUnit }
  | id_list   { $1 }

id_list:
    ID                  { AUnit([$1]) }
  | ID VERBAR id_list   { match $3 with AUnit v -> AUnit($1 :: v) | _ -> raise (Failure "Unit args mismatch") }

udecl_args:
    expr ID { CUnit($1, $2) }
  | ids_opt { $1 }

udecl:
    UNIT ID LBRACE udecl_args RBRACE { { uname=$2; prop=$4 } }

/* fdecl */
fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      locals=$6;
      body=$7
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  /* return */
  | RETURN expr SEMI                        { Return $2      }

expr:
    ILIT             { IntLit($1)             }
  | BLIT             { BoolLit($1)            }
  | FLIT             { FloatLit($1)           }
  | CLIT             { CharLit($1)            }
  | SLIT             { StrLit($1)             }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }


/* Scan only
   For testing
 */
tokenseq:
  tokens EOF { $1 }

tokens:
   /* nothing */ { [] }
 | one_token tokens { $1 :: $2 }

one_token:
  | LPAREN    { "(" }
  | RPAREN    { ")" }
  | LBRACK    { "[" }
  | RBRACK    { "]" }
  | LBRACE    { "{" }
  | RBRACE    { "}" }
  | COLON     { ":" }
  | SEMI      {  ";" }
  | COMMA     { "," }
  | DOT       { "." }
  /* Operators */
  | PLUS     { "PLUS" }
  | MINUS    { "MINUS" }
  | MUL      { "MUL" }
  | DIV      { "DIV" }
  | MOD      { "MOD" }
  | POW      { "POW" }
  | MATMUL   { "MATMUL" }
  | INC      { "INC" }
  | DEC      { "DEC" }
  | ASSIGN   { "ASSIGN" }
  | IASSIGN  { "IASSIGN" }
  | EQ       { "EQ" }
  | NEQ      { "NEQ" }
  | LT       { "LT" }
  | NOT      { "NOT" }
  | AND      { "AND" }
  | OR       { "OR" }
  /* Keywords */
  | VAR       { "VAR" }
  | CONST     { "CONST" }
  | STRUCT    { "STRUCT" }
  | UNIT      {"UNIT"}
  | VARIANT   {"VARIANT"}
  | IF        { "IF" }
  | ELSE      { "ELSE" }
  | SWITCH    {"SWITCH"}
  | MATCH     {"MATCH"}
  | CASE      {"CASE"}
  | WHILE     { "WHILE" }
  | FOR       {"FOR"}
  | CONTINUE  {"CONTINUE"}
  | BREAK     {"BREAK"}
  | FUNC      {"FUNC"}
  | RETURN    { "RETURN" }
  /* Types */
  | BOOL     { "BOOL" }
  | INT      { "INT" }
  | FLOAT    {"FLOAT"}
  | CHAR     {"CHAR"}
  | STRING   {"STRING"}
  | TENSOR   {"TENSOR"}
  /* Literals */
  | BLIT    { "BOOL(" ^ string_of_bool $1 ^ ")" }
  | ILIT    { "INT(" ^ string_of_int $1 ^ ")" }
  | FLIT    { "FLOAT(" ^ string_of_float $1 ^ ")" }
  | CLIT    { "CHAR(" ^ String.make 1 $1 ^ ")" }
  | SLIT    { "STRING(" ^ $1 ^ ")" }
  | ID      { "ID(" ^ $1 ^ ")" }
