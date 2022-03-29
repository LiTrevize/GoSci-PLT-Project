/* Ocamlyacc parser for GoSci */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE COLON DOT VERBAR
%token PLUS MINUS MUL DIV MOD POW MATMUL INC DEC ASSIGN IASSIGN
%token EQ GEQ NEQ LEQ GT LT NOT AND OR
%token VAR CONST STRUCT UNIT VARTYPE IF ELSE SWITCH MATCH CASE DEFAULT FALL WHILE FOR CONTINUE BREAK FUNC
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
%left LT GT GEQ LEQ
%left PLUS MINUS
%left MUL DIV MOD
%right NOT

%%

program:
  decls EOF { $1}

decls:
   /* nothing */    { ([], [], [], [])               }
 | vdecl SEMI decls { match $3 with (v, u, vt, f) -> (($1 :: v), u, vt, f) }
 | udecl decls      { match $2 with (v, u, vt, f) -> (v, ($1 :: u), vt, f) }
 | vtdecl decls     { match $2 with (v, u, vt, f) -> (v, u, ($1 :: vt), f) }
 | fdecl decls      { match $2 with (v, u, vt, f) -> (v, u, vt, ($1 :: f)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
  typ ID unit_expr_opt { ($1, $2, $3) }

typ:
    INT    { Int    }
  | BOOL   { Bool   }
  | FLOAT  { Float  }
  | CHAR   { Char   }
  | STRING { Str }

unit_expr_opt:
  /*nothing*/                                  { [] }
  | LBRACK ID RBRACK unit_expr_opt             { ($2, 1) :: $4 }
  | LBRACK ID ILIT RBRACK unit_expr_opt        { ($2, $3) :: $5 }
  | LBRACK ID MINUS ILIT RBRACK unit_expr_opt  { ($2, - $4) :: $6 }

units_opt:
  /*nothing*/ { BaseUnit }
  | unit_list   { $1 }

unit_list:
    ID                  { AUnit([$1]) }
  | ID VERBAR unit_list   { match $3 with AUnit v -> AUnit($1 :: v) | _ -> raise (Failure "Unit args mismatch") }

udecl_args:
    expr ID { CUnit($1, $2) }
  | units_opt { $1 }

udecl:
    UNIT ID LBRACE udecl_args RBRACE { ($2, $4) }

type_list:
    typ  { [$1] }
  | typ VERBAR type_list  { $1 :: $3 }

vtdecl:
    VARTYPE ID LBRACE type_list RBRACE  { ($2, $4) }

/* fdecl */
fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list statement_list RBRACE
  {
    {
      rtyp=(match $1 with (t, i, u) -> (t, u));
      fname=(match $1 with (t, i, u) -> i);
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



/*
  Missing:
  block
  declaration
  expr_list
  primary_expr

  multiple return value
*/

blocked:
    block       { $1 }
  | if_stmt     { $1 }
  | for_stmt    { $1 }
  | switch_stmt { $1 }
  | match_stmt  { $1 }


block:
    LBRACE statement_list RBRACE  { Block($2) }

expr_list:
    expr                  { [$1] }
  | expr COMMA expr_list  { $1::$3 }

statement_list:
  /* nothing */ { [] }
  | blocked statement_list           { $1::$2 }
  | statement SEMI statement_list   { $1::$3 }


statement:
    // declaration   { $1 }
    labeled_stmt  { $1 }
  | simple_stmt   { SimpleS($1) }
  | return_stmt   { $1 }
  | block         { $1 }
  | if_stmt       { $1 }
  | switch_stmt   { $1 }
  | match_stmt    { $1 }
  | for_stmt      { $1 }
  | loop_ctrl_stmt      { LoopS($1) }
  | fall_through_stmt   { $1 }


simple_stmt:
    // empty_stmt
  expression_stmt { $1 }
  // | inc_dec_stmt  { $1 }
  // | assignment    { $1 }
  // | short_var_decl 


loop_ctrl_stmt:
    break_stmt    { $1 }
  | continue_stmt { $1 }


labeled_stmt:
    label COLON statement { LabelS( $1, $3 ) }


label:
    ID { $1 }


// empty_stmt:
//  /*nothing*/


expression_stmt:
    expr { ExprS($1) }



// short_var_decl:
//     id_list IASSIGN expr_list


return_stmt:
    RETURN            { ReturnS([]) }
  | RETURN expr_list  { ReturnS($2) }


if_stmt:
    IF LPAREN expr RPAREN block                                 { IfS(None, $3, $5, None) }
  | IF LPAREN expr RPAREN block ELSE if_stmt                    { IfS(None, $3, $5, Some $7) }
  | IF LPAREN expr RPAREN block ELSE block                      { IfS(None, $3, $5, Some $7) }
  | IF LPAREN simple_stmt SEMI expr RPAREN block                { IfS(Some $3, $5, $7, None) }
  | IF LPAREN simple_stmt SEMI expr RPAREN block ELSE if_stmt   { IfS(Some $3, $5, $7, Some $9) }
  | IF LPAREN simple_stmt SEMI expr RPAREN block ELSE block     { IfS(Some $3, $5, $7, Some $9) }


switch_stmt:
    SWITCH LPAREN RPAREN LBRACE expr_case_list RBRACE                         { SwitchS(None, None, $5) }
  | SWITCH LPAREN simple_stmt SEMI RPAREN LBRACE expr_case_list RBRACE        { SwitchS(Some $3, None, $7) }
  | SWITCH LPAREN expr RPAREN LBRACE expr_case_list RBRACE                    { SwitchS(None, Some $3, $6) }
  | SWITCH LPAREN simple_stmt SEMI expr RPAREN LBRACE expr_case_list RBRACE   { SwitchS(Some $3, Some $5, $8) }


expr_case_list:
    expr_case_clause expr_case_list { $1::$2 }  // CaseS list
  | expr_case_clause { [$1] }
  

expr_case_clause:
    expr_switch_case COLON statement_list   { CaseS($1, $3) }


expr_switch_case:
    CASE expr_list  { $2 }
  | DEFAULT         { [] }


match_stmt:
    MATCH LPAREN ID IASSIGN expr RPAREN LBRACE match_clause_list RBRACE
    { MatchS(None, $3, $5, $8) }
  | MATCH LPAREN simple_stmt SEMI ID IASSIGN expr RPAREN LBRACE match_clause_list RBRACE
    { MatchS(Some $3, $5, $7, $10) }


match_clause_list:
    match_clause match_clause_list  { $1::$2 }  // MatchC list
  | match_clause                    { [$1] }


match_clause:
    match_case COLON statement_list   { MatchC($1, $3) }


match_case:
    CASE typ  { Some $2 }
  | DEFAULT   { None }


for_stmt:
    FOR LPAREN condition RPAREN block     { ForS(Condition($3), $5) }
  | FOR LPAREN for_clause RPAREN block    { ForS($3, $5) }
  | FOR LPAREN range_clause RPAREN block  { ForS($3, $5) }


condition:
    expr  { $1 }


for_clause:
    SEMI SEMI                                 { FClause(None, None, None) }
  | init_stmt SEMI SEMI                       { FClause(Some $1, None, None) }
  | SEMI condition SEMI                       { FClause(None, Some $2, None) }
  | SEMI SEMI post_stmt                       { FClause(None, None, Some $3) }
  | init_stmt SEMI condition SEMI             { FClause(Some $1, Some $3, None) }
  | init_stmt SEMI SEMI post_stmt             { FClause(Some $1, None, Some $4) }
  | SEMI condition SEMI post_stmt             { FClause(None, Some $2, Some $4) }
  | init_stmt SEMI condition SEMI post_stmt   { FClause(Some $1, Some $3, Some $5) }


init_stmt:
    simple_stmt   { SimpleS($1) }
  // | declaration   {  }


post_stmt:
    simple_stmt   { $1 }


range_clause:
    ID COLON expr   { RClause($1, $3) }


break_stmt:
    BREAK       { BreakS(None) }
  | BREAK label { BreakS(Some $2) }


continue_stmt:
    CONTINUE        { ContinueS(None) }
  | CONTINUE label  { ContinueS(Some $2) }


fall_through_stmt:
    FALL  { FallS(0) }


expr:
    ILIT unit_expr_opt  { IntLit($1, $2)         }
  | BLIT                { BoolLit($1)            }
  | FLIT unit_expr_opt  { FloatLit($1, $2)       }
  | CLIT                { CharLit($1)            }
  | SLIT                { StrLit($1)             }
  | ID                  { Id($1)                 }
  | expr PLUS   expr    { Binop($1, Add,   $3)   }
  | expr MINUS  expr    { Binop($1, Sub,   $3)   }
  | expr MUL    expr    { Binop($1, Mul,   $3)   }
  | expr DIV    expr    { Binop($1, Div,   $3)   }
  | expr MOD    expr    { Binop($1, Mod,   $3)   }
  | expr EQ     expr    { Binop($1, Equal, $3)   }
  | expr GEQ    expr    { Binop($1, Geq,   $3)   }
  | expr NEQ    expr    { Binop($1, Neq,   $3)   }
  | expr LEQ    expr    { Binop($1, Leq,   $3)   }
  | expr GT     expr    { Binop($1, Great, $3)   }
  | expr LT     expr    { Binop($1, Less,  $3)   }
  | expr AND    expr    { Binop($1, And,   $3)   }
  | expr OR     expr    { Binop($1, Or,    $3)   }
  | NOT    expr         { Unaop(Not,       $2)   }
  | MINUS  expr         { Unaop(Neg,       $2)   }
  | INC    expr         { Unaop(Inc,       $2)   }
  | DEC    expr         { Unaop(Dec,       $2)   }
  | ID ASSIGN expr      { Assign($1,       $3)   }
  | LPAREN expr RPAREN  { Paren(           $2)   }
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
  | SEMI      { ";" }
  | COMMA     { "," }
  | DOT       { "." }
  | VERBAR    { "|" }
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
  | VARTYPE   {"VARTYPE"}
  | IF        { "IF" }
  | ELSE      { "ELSE" }
  | SWITCH    {"SWITCH"}
  | MATCH     {"MATCH"}
  | CASE      {"CASE"}
  | DEFAULT   {"DEFAULT"}
  | FALL      {"FALLTHROUGH"}
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
