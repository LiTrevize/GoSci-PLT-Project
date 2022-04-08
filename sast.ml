(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = (typ * unit_expr) * sx

and sx =
  | SIntLit of int
  | SBoolLit of bool
  | SFloatLit of float
  | SCharLit of char
  | SStrLit of string
  | SId of string
  | SBinop of sexpr * bop * sexpr
  | SUnaop of uop * sexpr
  | SAssign of string * sexpr
  (* call *)
  | SCall of string * sexpr list

(* type ssimple_stmt = SExprS of sexpr *)
(* | SIncS of sexpr * uop
   | SAssignment of sexpr list * aop * sexpr list *)

type sstmt =
  | SLabelS of string * sstmt
  | SExprS of sexpr
  | SReturnS of sexpr list
  | SBlock of sstmt list
  | SIfS of sexpr option * sexpr * sstmt * sstmt option
  | SSwitchS of sexpr option * sexpr option * sswitch_case list
  | SMatchS of sexpr option * string * sexpr * smatch_clause list
  | SForS of sftype * sstmt
  | SLoopS of sloop_ctrl_stmt
  | SFallS of int

and sloop_ctrl_stmt =
  | SBreakS of string option
  | SContinueS of string option

and sswitch_case = SCaseS of sexpr list * sstmt list
and smatch_clause = SMatchC of typ option * sstmt list

and sftype =
  | SCondition of sexpr
  | SFClause of sstmt option * sexpr option * sexpr option
  | SRClause of string * sexpr

type sunit_prop =
  | SBaseUnit
  (* Concrete Unit *)
  | SCUnit of sexpr * string
  (* Abstract Unit *)
  | SAUnit of string list

type sunit_def = string * sunit_prop

type sutype_def =
  | SVarType of string * typ list
  | SStructType of string * bind list
  | STensorType of string * shapeList
  | SArrType of string * shapeList

(* func_def: ret_typ fname formals locals body *)
type sfunc_def =
  { srtyp : typ * unit_expr
  ; sfname : string
  ; sformals : bind list
  ; slocals : bind list
  ; sbody : sstmt list
  }

type sprogram = bind list * sunit_def list * sutype_def list * sfunc_def list

(* Pretty-printing functions *)
let rec string_of_sexpr (((t, u), e) : sexpr) =
  "("
  ^ string_of_typ t
  ^ string_of_unit_expr u
  ^ " : "
  ^ (match e with
    | SIntLit l -> string_of_int l
    | SBoolLit true -> "true"
    | SBoolLit false -> "false"
    | SFloatLit l -> string_of_float l
    | SCharLit l -> String.make 1 l
    | SStrLit l -> l
    | SId s -> s
    | SBinop (e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_bop o ^ " " ^ string_of_sexpr e2
    | SUnaop (o, e) -> string_of_uop o ^ " " ^ string_of_sexpr e
    | SAssign (v, e) -> v ^ " = " ^ string_of_sexpr e
    | SCall (f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")")
  ^ ")"
;;

let rec string_of_sstmt = function
  | SLabelS (label, stmt) -> label ^ string_of_sstmt stmt
  | SExprS expr ->
    string_of_sexpr expr ^ "\n"
    (* | SIncS(expr, uop) -> string_of_sexpr expr ^ string_of_uop uop ^ "\n"
             | SAssignment(exprs1, aop, exprs2) ->
               String.concat "" (List.map string_of_sexpr exprs1) ^ string_of_aop aop ^
               String.concat "" (List.map string_of_sexpr exprs2) ^ "\n" *)
  | SBlock stmts -> "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SReturnS expr ->
    (match expr with
    | [] -> "return\n"
    | ex -> "return " ^ String.concat "" (List.map string_of_sexpr ex) ^ "\n")
  | SIfS (sim, expr, stmt, stmt2) ->
    let ss =
      match sim with
      | None -> ""
      | Some v -> string_of_sexpr v ^ ";"
    in
    let blk = string_of_sstmt stmt in
    let el =
      match stmt2 with
      | None -> ""
      | Some v -> "else\n" ^ string_of_sstmt v
    in
    "if (" ^ ss ^ string_of_sexpr expr ^ ")\n" ^ blk ^ el
  | SSwitchS (sim, expr, sclist) ->
    let ss =
      match sim with
      | None -> ""
      | Some v -> string_of_sexpr v ^ ";"
    in
    let ex =
      match expr with
      | None -> ""
      | Some v -> string_of_sexpr v
    in
    "switch ("
    ^ ss
    ^ ex
    ^ ")\n{\n"
    ^ String.concat "" (List.map string_of_sswitch_case sclist)
    ^ "}\n"
  | SMatchS (sim, id, expr, mclist) ->
    let ss =
      match sim with
      | None -> ""
      | Some v -> string_of_sexpr v ^ ";"
    in
    let ex = string_of_sexpr expr in
    "match ("
    ^ ss
    ^ id
    ^ ":="
    ^ ex
    ^ ")\n{\n"
    ^ String.concat "" (List.map string_of_smatch_case mclist)
    ^ "}\n"
  | SForS (ftype, stmt) ->
    let f =
      match ftype with
      | SCondition c -> string_of_sexpr c
      | SFClause (stmt, expr, sstmt) ->
        let s =
          match stmt with
          | None -> ";"
          | Some v -> string_of_sstmt v ^ ";\n"
        in
        let e =
          match expr with
          | None -> ";"
          | Some v -> string_of_sexpr v ^ "\n;\n"
        in
        let ss =
          match sstmt with
          | None -> ""
          | Some v -> string_of_sexpr v
        in
        s ^ e ^ ss
      | SRClause (id, expr) -> id ^ ";" ^ string_of_sexpr expr
    in
    "for (\n" ^ f ^ ")\n" ^ string_of_sstmt stmt
  | SLoopS stmt ->
    (match stmt with
    | SBreakS b ->
      (match b with
      | None -> "break\n"
      | Some v -> "break " ^ v)
    | SContinueS c ->
      (match c with
      | None -> "continue\n"
      | Some v -> "continue " ^ v))
  | SFallS _ -> "fallthrough\n"

and string_of_sswitch_case = function
  | SCaseS ([], stmts) -> "default:\n" ^ String.concat "" (List.map string_of_sstmt stmts)
  | SCaseS (exprs, stmts) ->
    "case "
    ^ String.concat ", " (List.map string_of_sexpr exprs)
    ^ ":\n"
    ^ String.concat "" (List.map string_of_sstmt stmts)

and string_of_smatch_case = function
  | SMatchC (None, stmts) ->
    "default:\n" ^ String.concat "" (List.map string_of_sstmt stmts)
  | SMatchC (Some t, stmts) ->
    "case " ^ string_of_typ t ^ ":\n" ^ String.concat "" (List.map string_of_sstmt stmts)

and string_of_sfdecl (fdecl : sfunc_def) =
  string_of_rtyp fdecl.srtyp
  ^ " "
  ^ fdecl.sfname
  ^ "("
  ^ String.concat ", " (List.map string_of_bind fdecl.sformals)
  ^ ")\n{\n"
  ^ String.concat "" (List.map string_of_vdecl fdecl.slocals)
  ^ String.concat "" (List.map string_of_sstmt fdecl.sbody)
  ^ "}\n"
;;

let string_of_sudecl (udecl : sunit_def) =
  "unit "
  ^ fst udecl
  ^ " {\n"
  ^ (match snd udecl with
    | SBaseUnit -> ""
    | SCUnit (e, id) -> string_of_sexpr e ^ " " ^ id ^ "\n"
    | SAUnit ids -> String.concat " | " ids ^ "\n")
  ^ "}\n"
;;

let string_of_sutype (utype : sutype_def) =
  match utype with
  | SVarType (name, type_list) ->
    "VarType "
    ^ name
    ^ " {\n"
    ^ String.concat " | " (List.map string_of_typ type_list)
    ^ "\n}\n"
  | SStructType (name, bind_list) ->
    "StructType("
    ^ name
    ^ ") {\n"
    ^ String.concat " | " (List.map string_of_bind bind_list)
    ^ "\n}\n"
  | STensorType (name, shape_list) ->
    "TensorType" ^ string_of_shape shape_list ^ name ^ "\n"
  | SArrType (name, shape_list) -> "ArrType" ^ string_of_shape shape_list ^ name ^ "\n"
;;

let string_of_sprogram ((vars, units, utypes, funcs) : sprogram) =
  "\n\nSementically checked program: \n\n"
  ^ String.concat "" (List.map string_of_vdecl vars)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_sudecl units)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_sutype utypes)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_sfdecl funcs)
;;
