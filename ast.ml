(* Abstract Syntax Tree and functions for printing it *)

type bop = Add | Sub | Equal | Neq | Less | And | Or

type typ = Int | Bool | Float | Char | Str

type expr =
    IntLit of int
  | BoolLit of bool
  | FloatLit of float
  | CharLit of char
  | StrLit of string
  | Id of string
  | Binop of expr * bop * expr
  | Assign of string * expr
  (* function call *)
  | Call of string * expr list

type unit_prop =
    BaseUnit
  (* Concrete Unit *)
  | CUnit of expr * string
  (* Abstract Unit *)
  | AUnit of string list

type unit_def = {
  uname: string;
  prop: unit_prop
}

type vtype_def = string * typ list

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  (* return *)
  | Return of expr

(* int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type program = bind list * unit_def list * vtype_def list * func_def list

(* Pretty-printing functions *)
let string_of_bop = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
  | Or -> "||"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | FloatLit(l) -> string_of_float l
  | CharLit(l) -> String.make 1 l
  | StrLit(l) -> l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_bop o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Char -> "char"
  | Str -> "string"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_udecl udecl =
  "unit " ^ udecl.uname ^ " {\n" ^
  (match udecl.prop with
  | BaseUnit -> ""
  | CUnit (e, id) -> (string_of_expr e) ^ " " ^ id ^ "\n"
  | AUnit ids -> String.concat " | " ids ^ "\n")
  ^ "}\n"

let string_of_vtype vtype = 
  "vartype " ^ (fst vtype) ^ " {\n" ^ String.concat " | " (List.map string_of_typ (snd vtype)) ^ "\n}\n"

let string_of_program (vars, units, vtypes, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_udecl units) ^ "\n" ^
  String.concat "\n" (List.map string_of_vtype vtypes) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)


(* for testing *)
type tokenseq = string list

let string_of_tokenseq l =
  let f s e = match e with
  | ";" -> s ^ ";\n"
  | "{" -> s ^ "{\n"
  | "}" -> s ^ "}\n"
  | _ -> s ^ e ^ " " in
  "Scanned program:\n" ^ (List.fold_left f "" l)

(* let string_of_tokenseq l =
  let rec strn s n = match n with
  | 0 -> ""
  | 1 -> s
  | _ -> s ^ strn s (n-1) in
  let f t e = match t with
  | s, i ->
    if e = "{" then (s ^ e ^ "\n" ^ strn "    " (i+1), i+1)
    else if e = "}" then (s ^ "\b\b\b\b" ^ strn "    " (i-1) ^ e ^ "\n" ^ strn "\t" (i-1), i-1)
    else if e = ";" then (s ^  e ^ "\n" ^ strn "    " i, i)
    else (s ^ e ^ " ", i) in
  "Scanned program:\n" ^ (fst (List.fold_left f ("",0) l)) *)