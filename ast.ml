(* Abstract Syntax Tree and functions for printing it *)

type bop =
  | Add
  | Sub
  | Mul
  | Div
  | Pow
  | Mod
  | Equal
  | And
  | Or
  | Geq
  | Neq
  | Leq
  | Great
  | Less

type uop =
  | Not
  | Neg

type iodop =
  | Inc
  | Dec

type typ =
  | Int
  | Bool
  | Float
  | Char
  | Str
  | UserType of string

(*
  unit term: (unit_name, power)
  e.g. [m -2]
*)
type unit_term = string * int
type unit_expr = unit_term list

type expr =
  | IntLit of int * unit_expr
  | BoolLit of bool
  | FloatLit of float * unit_expr
  | CharLit of char
  | StrLit of string
  | Id of string
  | FieldLit of string * string
  | StructLit of string * expr list
  | Binop of expr * bop * expr
  | Unaop of uop * expr
  | IoDop of expr * iodop
  | Assign of string * expr
  | AssignField of string * string * expr (* var field expr *)
  | Paren of expr
  (* function call *)
  | Call of string * expr list

type unit_prop =
  | BaseUnit
  (* Concrete Unit *)
  | CUnit of expr * string
  (* Abstract Unit *)
  | AUnit of string list

(* unit_def: (name, unit_prop) *)
type unit_def = string * unit_prop
(* type simple_stmt = ExprS of expr *)
(* | IncS of expr * uop
   | Assignment of expr list * aop * expr list *)

type stmt =
  (* DeclS of decl *)
  | LabelS of string * stmt
  | ExprS of expr
  | ReturnS of expr list
  | Block of stmt list
  | IfS of expr option * expr * stmt * stmt option
  | SwitchS of expr option * expr option * switch_case list
  | MatchS of expr option * string * expr * match_clause list
  | ForS of ftype * stmt
  | LoopS of loop_ctrl_stmt
  | FallS of int

and loop_ctrl_stmt =
  | BreakS of string option
  | ContinueS of string option

and switch_case = CaseS of expr list * stmt list
and match_clause = MatchC of typ option * stmt list

and ftype =
  | Condition of expr
  | FClause of stmt option * expr option * expr option
  | RClause of string * expr

type bind = typ * int list * string * unit_expr * expr option

(* func_def: ret_typ fname formals locals body *)
type func_def =
  { rtyp : typ * unit_expr
  ; fname : string
  ; formals : bind list
  ; locals : bind list
  ; body : stmt list
  }

type shapeList = int list

type utype_def =
  | VarType of string * typ list
  | StructType of string * bind list

(* program = (globals, units, vartypes, functions) *)
type program = bind list * unit_def list * utype_def list * func_def list

(* Pretty-printing functions *)
let string_of_bop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Pow -> "^"
  | Mod -> "%"
  | Equal -> "=="
  | Geq -> ">="
  | Neq -> "!="
  | Leq -> "<="
  | Great -> ">"
  | Less -> "<"
  | And -> "&&"
  | Or -> "||"
;;

let string_of_unit_term (name, exp) = "[" ^ name ^ " " ^ string_of_int exp ^ "]"
let string_of_unit_expr uexpr = String.concat "" (List.map string_of_unit_term uexpr)

let string_of_uop = function
  | Not -> "!"
  | Neg -> "-"
;;

let string_of_iodop = function
  | Inc -> "++"
  | Dec -> "--"
;;

let rec string_of_expr = function
  | IntLit (l, u) -> string_of_int l ^ string_of_unit_expr u
  | BoolLit true -> "true"
  | BoolLit false -> "false"
  | FloatLit (l, u) -> string_of_float l ^ string_of_unit_expr u
  | CharLit l -> String.make 1 l
  | StrLit l -> l
  | Id s -> s
  | StructLit (name, expr_list) ->
    name ^ "{" ^ String.concat ", " (List.map string_of_expr expr_list) ^ "}"
  | FieldLit (v, f) -> v ^ "." ^ f
  | Binop (e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_bop o ^ " " ^ string_of_expr e2
  | Unaop (o, e) -> string_of_uop o ^ string_of_expr e
  | IoDop (e, op) -> string_of_expr e ^ " " ^ string_of_iodop op
  | Assign (v, e) -> v ^ " = " ^ string_of_expr e
  | AssignField (v, f, e) -> v ^ "." ^ f ^ " = " ^ string_of_expr e
  | Paren e -> "(" ^ string_of_expr e ^ ")"
  | Call (f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
;;

let rec string_of_stmt = function
  | LabelS (label, stmt) -> label ^ string_of_stmt stmt
  | ExprS expr -> string_of_expr expr ^ "\n"
  | Block stmts -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | ReturnS expr ->
    (match expr with
    | [] -> "return\n"
    | ex -> "return " ^ String.concat "" (List.map string_of_expr ex) ^ "\n")
  | IfS (sstmt, expr, stmt, stmt2) ->
    let ss =
      match sstmt with
      | None -> ""
      | Some v -> string_of_expr v ^ ";"
    in
    let blk = string_of_stmt stmt in
    let el =
      match stmt2 with
      | None -> ""
      | Some v -> "else\n" ^ string_of_stmt v
    in
    "if (" ^ ss ^ string_of_expr expr ^ ")\n" ^ blk ^ el
  | SwitchS (sstmt, expr, sclist) ->
    let ss =
      match sstmt with
      | None -> ""
      | Some v -> string_of_expr v ^ ";"
    in
    let ex =
      match expr with
      | None -> ""
      | Some v -> string_of_expr v
    in
    "switch ("
    ^ ss
    ^ ex
    ^ ")\n{\n"
    ^ String.concat "" (List.map string_of_switch_case sclist)
    ^ "}\n"
  | MatchS (sstmt, id, expr, mclist) ->
    let ss =
      match sstmt with
      | None -> ""
      | Some v -> string_of_expr v ^ ";"
    in
    let ex = string_of_expr expr in
    "match ("
    ^ ss
    ^ id
    ^ ":="
    ^ ex
    ^ ")\n{\n"
    ^ String.concat "" (List.map string_of_match_case mclist)
    ^ "}\n"
  | ForS (ftype, stmt) ->
    let f =
      match ftype with
      | Condition c -> string_of_expr c
      | FClause (stmt, expr, sstmt) ->
        let s =
          match stmt with
          | None -> ";"
          | Some v -> string_of_stmt v ^ ";\n"
        in
        let e =
          match expr with
          | None -> ";"
          | Some v -> string_of_expr v ^ "\n;\n"
        in
        let ss =
          match sstmt with
          | None -> ""
          | Some v -> string_of_expr v
        in
        s ^ e ^ ss
      | RClause (id, expr) -> id ^ ";" ^ string_of_expr expr
    in
    "for (\n" ^ f ^ ")\n" ^ string_of_stmt stmt
  | LoopS stmt ->
    (match stmt with
    | BreakS b ->
      (match b with
      | None -> "break\n"
      | Some v -> "break " ^ v)
    | ContinueS c ->
      (match c with
      | None -> "continue\n"
      | Some v -> "continue " ^ v))
  | FallS _ -> "fallthrough\n"

and string_of_switch_case = function
  | CaseS ([], stmts) -> "default:\n" ^ String.concat "" (List.map string_of_stmt stmts)
  | CaseS (exprs, stmts) ->
    "case "
    ^ String.concat "" (List.map string_of_expr exprs)
    ^ ":\n"
    ^ String.concat "" (List.map string_of_stmt stmts)

and string_of_match_case = function
  | MatchC (None, stmts) ->
    "default:\n" ^ String.concat "" (List.map string_of_stmt stmts)
  | MatchC (Some t, stmts) ->
    "case " ^ string_of_typ t ^ ":\n" ^ String.concat "" (List.map string_of_stmt stmts)

and string_of_typ = function
  | Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Char -> "char"
  | Str -> "string"
  | UserType type_name -> type_name
;;

let rec string_of_shape (sh : int list) =
  match sh with
  | [] -> ""
  | hd :: tl -> "[" ^ string_of_int hd ^ "]" ^ string_of_shape tl
;;

let string_of_bind ((t, sh, id, units, init_expr) : bind) =
  string_of_typ t
  ^ string_of_shape sh
  ^ " "
  ^ id
  ^ " "
  ^ String.concat "" (List.map string_of_unit_term units)
  ^
  match init_expr with
  | Some e -> " = " ^ string_of_expr e
  | None -> ""
;;

let rec string_of_shape = function
  | [] -> " "
  | hd :: tl -> "[" ^ string_of_int hd ^ "]" ^ string_of_shape tl
;;

let string_of_vdecl (bnd : bind) = string_of_bind bnd ^ ";\n"

let string_of_rtyp (rtyp : typ * unit_expr) =
  string_of_typ (fst rtyp) ^ " " ^ string_of_unit_expr (snd rtyp)
;;

let string_of_fdecl (fdecl : func_def) =
  string_of_rtyp fdecl.rtyp
  ^ " "
  ^ fdecl.fname
  ^ "("
  ^ String.concat ", " (List.map string_of_bind fdecl.formals)
  ^ ")\n{\n"
  ^ String.concat "" (List.map string_of_vdecl fdecl.locals)
  ^ String.concat "" (List.map string_of_stmt fdecl.body)
  ^ "}\n"
;;

let string_of_udecl (udecl : unit_def) =
  "unit "
  ^ fst udecl
  ^ " {\n"
  ^ (match snd udecl with
    | BaseUnit -> ""
    | CUnit (e, id) -> string_of_expr e ^ " " ^ id ^ "\n"
    | AUnit ids -> String.concat " | " ids ^ "\n")
  ^ "}\n"
;;

let string_of_utype (utype : utype_def) =
  match utype with
  | VarType (name, type_list) ->
    "vartype "
    ^ name
    ^ " {\n"
    ^ String.concat " | " (List.map string_of_typ type_list)
    ^ "\n}\n"
  | StructType (name, bind_list) ->
    "structType"
    ^ name
    ^ " {\n"
    ^ String.concat " " (List.map string_of_bind bind_list)
    ^ "\n}\n"
;;

let string_of_program ((vars, units, utypes, funcs) : program) =
  "\n\nParsed program: \n\n"
  ^ String.concat "" (List.map string_of_vdecl vars)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_udecl units)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_utype utypes)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_fdecl funcs)
;;

(* for testing *)
type tokenseq = string list

let string_of_tokenseq l =
  let f s e =
    match e with
    | ";" -> s ^ ";\n"
    | "{" -> s ^ "{\n"
    | "}" -> s ^ "}\n"
    | _ -> s ^ e ^ " "
  in
  "Scanned program:\n" ^ List.fold_left f "" l
;;

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
