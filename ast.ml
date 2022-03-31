(* Abstract Syntax Tree and functions for printing it *)

type bop = Add | Sub | Equal | Neq | Less | And | Or

type uop = Inc | Dec

type aop = As | Pas | Mias | Das | Muas

type typ = Int | Bool | Float | Char | Str | UserType of string

(*
  unit term: (unit_name, power)
  e.g. [m -2]
*)
type unit_term = string * int

type unit_expr = unit_term list

type expr =
    IntLit of int * unit_expr
  | BoolLit of bool
  | FloatLit of float * unit_expr
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

(* unit_def: (name, unit_prop) *)
type unit_def = string * unit_prop



type simple_stmt = 
    ExprS of expr
  | IncS of expr * uop
  | Assignment of expr list * aop * expr list

type stmt = 
    (* DeclS of decl *)
  | LabelS of string * stmt
  | SimpleS of simple_stmt
  | ReturnS of expr list
  | Block of stmt list
  | IfS of simple_stmt option * expr * stmt * stmt option
  | SwitchS of simple_stmt option * expr option * switch_case list
  (* | MatchS of simple_stmt option * string * expr * match_clause list *)
  | ForS of ftype * stmt
  | LoopS of loop_ctrl_stmt
  | FallS of int

and loop_ctrl_stmt = 
    BreakS of string option
  | ContinueS of string option

and switch_case = 
  CaseS of expr list * stmt list

and match_clause = 
  MatchC of typ list * stmt list

and ftype = 
    Condition of expr
  | FClause of stmt option * expr option * simple_stmt option
  | RClause of string * expr


(* int x [m][s -2]: name binding *)
type bind = typ * string * unit_expr

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ * unit_expr;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type shapeList = int list

type vtype_def = 
VarType of string * typ list
| StructType of string * bind list
| TensorType of string * shapeList 
| ArrType of string * shapeList


(* program = (globals, units, vartypes, functions) *)
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

  
let string_of_unit_term (name, exp) = "[" ^ name ^ " " ^ string_of_int exp ^ "]"

let string_of_unit_expr uexpr = String.concat "" (List.map string_of_unit_term uexpr)

let string_of_uop = function
    Inc -> "++"
  | Dec -> "--"

let string_of_aop = function
    As -> "="
  | Pas -> "+="
  | Mias -> "-="
  | Das -> "/="
  | Muas -> "*="

let rec string_of_expr = function
    IntLit(l, u) -> string_of_int l ^ string_of_unit_expr u
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | FloatLit(l, u) -> string_of_float l ^ string_of_unit_expr u
  | CharLit(l) -> String.make 1 l
  | StrLit(l) -> l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_bop o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"


let rec string_of_stmt = function
    LabelS(label, stmt) -> label ^ string_of_stmt stmt
  | SimpleS(stmt) ->
    begin
      match stmt with
      | ExprS(expr) -> string_of_expr expr ^ "\n"
      | IncS(expr, uop) -> string_of_expr expr ^ string_of_uop uop ^ "\n"
      | Assignment(exprs1, aop, exprs2) ->
        String.concat "" (List.map string_of_expr exprs1) ^ string_of_aop aop ^
        String.concat "" (List.map string_of_expr exprs2) ^ "\n"
    end
  | Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | ReturnS(expr) -> 
    begin
      match expr with
      | [] -> "return\n"
      | ex -> "return " ^ String.concat "" (List.map string_of_expr ex) ^ "\n"
    end
  | IfS(sstmt, expr, stmt, stmt2) -> 
    let ss = match sstmt with None -> "" | Some v -> string_of_stmt (SimpleS(v)) ^ ";" in
    let el = match stmt2 with None -> "" | Some v -> "else" ^ string_of_stmt v in
    "if (" ^ ss ^ string_of_expr expr ^ ")\n" ^ el
  | SwitchS(sstmt, expr, sclist) ->
    let ss = match sstmt with None -> "" | Some v -> string_of_stmt (SimpleS(v)) ^ ";" in
    let ex = match expr with None -> "" | Some v -> string_of_expr (v) in
    "switch (" ^ ss ^ ex ^ ")\n{\n" ^ String.concat "" (List.map string_of_switch_case sclist) ^ "\n}\n"
  | ForS(ftype, stmt) -> 
    let f = 
      begin
      match ftype with
      | Condition(c) -> string_of_expr c
      | FClause(stmt, expr, sstmt) -> 
        let s = match stmt with None -> ";" | Some v -> string_of_stmt (v) ^ ";" in
        let e = match expr with None -> ";" | Some v -> string_of_expr (v) ^ ";" in
        let ss = match sstmt with None -> "" | Some v -> string_of_stmt (SimpleS(v)) in
        s ^ e ^ ss
      | RClause(id, expr) -> id ^ ";" ^ string_of_expr expr 
      end in
    "for (" ^ f ^ ")\n" ^ string_of_stmt stmt
  | LoopS(stmt) -> 
    begin
      match stmt with
      | BreakS(b) -> (match b with None -> "break\n" | Some v -> "break " ^ v)
      | ContinueS(c) -> (match c with None -> "continue\n" | Some v -> "continue " ^ v)
    end
  (* | MatchS(sstmt, id, expr, mclist) -> "Not implemented.\n" *)
  | FallS(_) -> "fallthrough\n"

and string_of_switch_case = function
    CaseS([], stmts) -> "default:\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "\n"
  | CaseS(exprs, stmts) -> "case " ^ String.concat "" (List.map string_of_expr exprs) ^ 
    ":\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "\n"


let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Char -> "char"
  | Str -> "string"
  | UserType(type_name) -> "UserType(" ^ type_name ^")" 

let string_of_bind ((t, id, units):bind) =
  string_of_typ t ^ " " ^ id ^ " " ^ String.concat "" (List.map string_of_unit_term units)
let rec string_of_shape  = function
  [] -> " "
  | hd::tl -> "[" ^ string_of_int hd ^"]" ^ string_of_shape tl



let string_of_vdecl (bnd:bind) =
  string_of_bind bnd ^ ";\n"

let string_of_rtyp (rtyp:typ*unit_expr) = string_of_typ (fst rtyp) ^ " " ^ string_of_unit_expr (snd rtyp)

let string_of_fdecl (fdecl:func_def) =
  string_of_rtyp fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_bind fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_udecl (udecl:unit_def) =
  "unit " ^ fst udecl ^ " {\n" ^
  (match snd udecl with
  | BaseUnit -> ""
  | CUnit (e, id) -> (string_of_expr e) ^ " " ^ id ^ "\n"
  | AUnit ids -> String.concat " | " ids ^ "\n")
  ^ "}\n"

let string_of_vtype (vtype:vtype_def) = 
  match vtype with 
  | VarType (name, type_list) -> "vartype " ^ name ^ " {\n" ^ String.concat " | " (List.map string_of_typ type_list) ^ "\n}\n"
  | StructType(name, bind_list) -> "structType" ^ name ^ " {\n" ^ String.concat " " (List.map string_of_bind bind_list) ^ "\n}\n"
  | TensorType(name, shape_list) -> "tensorType" ^ string_of_shape shape_list ^ name ^ "\n"
  | ArrType(name, shape_list) -> "arrType" ^ string_of_shape shape_list ^ name ^ "\n"

let string_of_program ((vars, units, vtypes, funcs):program) =
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