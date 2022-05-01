(* Ocamllex scanner for GoSci *)

{ open Gosciparse }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let letter_lower = ['a'-'z']
let letter_upper = ['A'-'Z']
let special = [' ' ',' '.' '!' '?']
let char_lit = digit | letter | special
let exp = ('e'|'E') ('+'|'-')? digit+
let float_lit = digit+ '.' digit* exp? | digit+ exp | '.' digit+ exp?

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment 0 lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LBRACK }
| ']'      { RBRACK }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ':'      { COLON }
| ';'      { SEMI }
| ','      { COMMA }
| '.'      { DOT }
| '|'      { VERBAR }
(* Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { MUL }
| '/'      { DIV }
| '%'      { MOD }
| '^'      { POW }
| '@'      { MATMUL }
| "++"     { INC }
| "--"     { DEC }
| '='      { ASSIGN }
| ":="     { IASSIGN }
| "=="     { EQ }
| ">="     { GEQ }
| "!="     { NEQ }
| "<="     { LEQ }
| '>'      { GT }
| '<'      { LT }
| '!'      { NOT }
| "&&"     { AND }
| "||"     { OR }
(* Keywords *)
| "var"       { VAR }
| "const"     { CONST }
| "struct"    { STRUCT }
| "unit"      { UNIT }
| "vartype"   { VARTYPE }
| "if"        { IF }
| "else"      { ELSE }
| "switch"    { SWITCH }
| "match"     { MATCH }
| "case"      { CASE }
| "default"   { DEFAULT }
| "fallthrough" { FALL }
| "while"     { WHILE }
| "for"       { FOR }
| "continue"  { CONTINUE }
| "break"     { BREAK }
| "func"      { FUNC }
| "return"    { RETURN }
(* Types *)
| "bool"      { BOOL }
| "int"       { INT }
| "float"     { FLOAT }
| "char"      { CHAR }
| "string"    { STRING }
| "tensor"    { TENSOR }
(* Literals *)
| "true"      { BLIT(true)  }
| "false"     { BLIT(false) }
| digit+ as lem                      { ILIT(int_of_string lem) }
| float_lit as lem                   { FLIT(float_of_string lem) }
| '\'' (char_lit? as lem) '\''       { CLIT(lem.[0]) }
| '\"' (char_lit* as lem) '\"'       { SLIT(lem) }
| letter_lower (digit | letter)* as lem    { LID(lem) }
| letter_upper (digit | letter)* as lem    { UID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment level = parse
  "*/" { if level == 0 then token lexbuf else comment (level - 1) lexbuf}
| "/*" { comment (level + 1) lexbuf }
| _    { comment level lexbuf }
