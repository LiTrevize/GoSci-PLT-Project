(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = (typ * unit_expr) * sx
and sx =
    SIntLit of int
  | SBoolLit of bool
  | SFloatLit of float
  | SCharLit of char
  | SStrLit of string
  | SId of string
  | SBinop of sexpr * bop * sexpr
  | SAssign of string * sexpr
  (* call *)
  | SCall of string * sexpr list


type ssimple_stmt = 
    SExprS of sexpr
  | SIncS of sexpr * uop
  | SAssignment of sexpr list * aop * sexpr list

type sstmt = 
    SLabelS of string * sstmt
  | SSimpleS of ssimple_stmt
  | SReturnS of sexpr list
  | SBlock of sstmt list
  | SIfS of ssimple_stmt option * sexpr * sstmt * sstmt option
  | SSwitchS of ssimple_stmt option * sexpr option * sswitch_case list
  (* | SMatchS of ssimple_stmt option * string * sexpr * smatch_clause list *)
  | SForS of sftype * sstmt
  | SLoopS of sloop_ctrl_stmt
  | SFallS of int

and sloop_ctrl_stmt = 
    SBreakS of string option
  | SContinueS of string option

and sswitch_case = 
  SCaseS of sexpr list * sstmt list

and smatch_clause = 
  SMatchC of typ list * sstmt list

and sftype = 
    SCondition of sexpr
  | SFClause of sstmt option * sexpr option * ssimple_stmt option
  | SRClause of string * sexpr


type sunit_prop =
    SBaseUnit
  (* Concrete Unit *)
  | SCUnit of sexpr * string
  (* Abstract Unit *)
  | SAUnit of string list

type sunit_def = string * sunit_prop

type svtype_def = 
SVarType of string * typ list
| SStructType of string * bind list

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ * unit_expr;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = bind list * sunit_def list * svtype_def list * sfunc_def list



(* Pretty-printing functions *)
let rec string_of_sexpr (((t, u), e):sexpr) =
  "(" ^ string_of_typ t ^ string_of_unit_expr u ^ " : " ^ (match e with
        SIntLit(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SFloatLit(l) -> string_of_float l
      | SCharLit(l) -> String.make 1 l
      | SStrLit(l) -> l
      | SId(s) -> s
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_bop o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    ) ^ ")"



let rec string_of_sstmt = function
    SLabelS(label, stmt) -> label ^ string_of_sstmt stmt
  | SSimpleS(stmt) ->
    begin
      match stmt with
      | SExprS(expr) -> string_of_sexpr expr ^ "\n"
      | SIncS(expr, uop) -> string_of_sexpr expr ^ string_of_uop uop ^ "\n"
      | SAssignment(exprs1, aop, exprs2) ->
        String.concat "" (List.map string_of_sexpr exprs1) ^ string_of_aop aop ^
        String.concat "" (List.map string_of_sexpr exprs2) ^ "\n"
    end
  | SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SReturnS(expr) -> 
    begin
      match expr with
      | [] -> "return\n"
      | ex -> "return " ^ String.concat "" (List.map string_of_sexpr ex) ^ "\n"
    end
  | SIfS(sim, expr, stmt, stmt2) -> 
    let ss = match sim with None -> "" | Some v -> string_of_sstmt (SSimpleS(v)) ^ ";" in
    let el = match stmt2 with None -> "" | Some v -> "else" ^ string_of_sstmt v in
    "if (" ^ ss ^ string_of_sexpr expr ^ ")\n" ^ el
  | SSwitchS(sim, expr, sclist) ->
    let ss = match sim with None -> "" | Some v -> string_of_sstmt (SSimpleS(v)) ^ ";" in
    let ex = match expr with None -> "" | Some v -> string_of_sexpr v in
    "switch (" ^ ss ^ ex ^ ")\n{\n" ^ String.concat "" (List.map string_of_sswitch_case sclist) ^ "\n}\n"
  | SForS(ftype, stmt) -> 
    let f = 
      begin
      match ftype with
      | SCondition(c) -> string_of_sexpr c
      | SFClause(stmt, expr, sstmt) -> 
        let s = (match stmt with None -> ";" | Some v -> string_of_sstmt v ^ ";") in
        let e = (match expr with None -> ";" | Some v -> string_of_sexpr v ^ ";") in
        let ss = (match sstmt with None -> "" | Some v -> string_of_sstmt (SSimpleS(v))) in
        s ^ e ^ ss
      | SRClause(id, expr) -> id ^ ";" ^ string_of_sexpr expr 
      end in
    "for (" ^ f ^ ")\n" ^ string_of_sstmt stmt
  | SLoopS(stmt) -> 
    begin
      match stmt with
      | SBreakS(b) -> (match b with None -> "break\n" | Some v -> "break " ^ v)
      | SContinueS(c) -> (match c with None -> "continue\n" | Some v -> "continue " ^ v)
    end
  (* | SMatchS(sstmt, id, expr, mclist) -> "Not implemented.\n" *)
  | SFallS(_) -> "fallthrough\n"

and string_of_sswitch_case = function
    SCaseS([], stmts) -> "default:\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "\n"
  | SCaseS(exprs, stmts) -> "case " ^ String.concat "" (List.map string_of_sexpr exprs) ^ 
    ":\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "\n"


let string_of_sfdecl (fdecl:sfunc_def) =
  string_of_rtyp fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map string_of_bind fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sudecl (udecl:sunit_def) =
  "unit " ^ fst udecl ^ " {\n" ^
  (match snd udecl with
  | SBaseUnit -> ""
  | SCUnit (e, id) -> (string_of_sexpr e) ^ " " ^ id ^ "\n"
  | SAUnit ids -> String.concat " | " ids ^ "\n")
  ^ "}\n"

let string_of_svtype (vtype:svtype_def) = 
  match vtype with 
  | SVarType (name, type_list) -> "vartype " ^ name ^ " {\n" ^ String.concat " | " (List.map string_of_typ type_list) ^ "\n}\n"
  | SStructType(name, bind_list) -> "structtype" ^ name ^ " {\n" ^ String.concat " | " (List.map string_of_bind bind_list) ^ "\n}\n"


let string_of_sprogram ((vars, units, vtypes, funcs):sprogram) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sudecl units) ^ "\n" ^
  String.concat "\n" (List.map string_of_svtype vtypes) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
