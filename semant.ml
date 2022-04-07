(* Semantic checking for the MicroC compiler *)

open Ast
open Sast
module StringMap = Map.Make (String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check ((globals, units, utypes, functions) : program) =
  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (typ * string * unit_expr) list) =
    let rec dups = function
      | [] -> ()
      | (_, n1, _) :: (_, n2, _) :: _ when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in
    dups (List.sort (fun (_, a, _) (_, b, _) -> compare a b) binds)
  in
  (* Make sure no globals duplicate *)
  let _ = check_binds "global" globals in
  (* Create user type table *)
  let add_type tuple = function
    | VarType (name, type_list) -> StringMap.add name type_list (fst tuple), snd tuple
    | StructType (name, bind_list) -> fst tuple, StringMap.add name bind_list (snd tuple)
    | _ -> tuple
  in
  let global_type_tuple =
    List.fold_left add_type (StringMap.empty, StringMap.empty) utypes
  in
  let global_vartypes = fst global_type_tuple in
  let global_structs = snd global_type_tuple in
  (* Create global variable symbol table *)
  let check_usertyped_var type_name =
    if (not (StringMap.mem type_name global_vartypes))
       && not (StringMap.mem type_name global_structs)
    then raise (Failure ("Type name (" ^ type_name ^ ")" ^ " is not defiend!"))
  in
  let add_var m = function
    | (UserType type_name as ty), name, uexpr ->
      check_usertyped_var type_name;
      StringMap.add name (ty, uexpr) m
    | ty, name, uexpr -> StringMap.add name (ty, uexpr) m
  in
  let global_vars = List.fold_left add_var StringMap.empty globals in
  let type_of_identifier s m =
    try fst (StringMap.find s m) with
    | Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  let unit_of_identifier s m =
    try snd (StringMap.find s m) with
    | Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
  let check_type_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in
  let check_unit_assign lu ru err =
    (* if lvaluet = rvaluet then lvaluet else raise (Failure err) *)
    if lu = ru || ru = []
    then lu
    else
      raise
        (Failure
           ("Incompatible unit: "
           ^ string_of_unit_expr lu
           ^ " and "
           ^ string_of_unit_expr ru))
  in
  let unit_convert u1 u2 =
    (* TODO: real convert *)
    if u1 = u2
    then u1
    else
      raise
        (Failure
           (string_of_unit_expr u2 ^ " cannot be converted to " ^ string_of_unit_expr u1))
  in
  let unit_simplify u =
    let update m item =
      let old =
        try StringMap.find (fst item) m with
        | Not_found -> 0
      in
      StringMap.add (fst item) (old + snd item) m
    in
    let m = List.fold_left update StringMap.empty u in
    let fold_unit (lst, m) item =
      if not (StringMap.mem (fst item) m)
      then lst, m
      else (
        let n = StringMap.find (fst item) m in
        if n = 0
        then lst, StringMap.remove (fst item) m
        else (
          let newlst = (fst item, n) :: lst in
          newlst, StringMap.remove (fst item) m))
    in
    List.rev (fst (List.fold_left fold_unit ([], m) u))
  in
  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    StringMap.add
      "print"
      { rtyp = Int, []
      ; fname = "print"
      ; formals = [ Int, "x", [] ]
      ; locals = []
      ; body = []
      }
      StringMap.empty
  in
  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *) in
    match fd with
    (* No duplicate functions or redefinitions of built-ins *)
    | _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ -> StringMap.add n fd map
  in
  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions in
  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls with
    | Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  let _ = find_func "main" in
  (* Ensure "main" is defined *)
  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;
    (* Build local symbol table of variables for this function *)
    let symbols =
      List.fold_left add_var StringMap.empty (globals @ func.formals @ func.locals)
    in
    (* Return a variable from our local symbol table *)

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = function
      | IntLit (l, u) -> (Int, u), SIntLit l
      | BoolLit l -> (Bool, []), SBoolLit l
      | FloatLit (l, u) -> (Float, u), SFloatLit l
      | CharLit l -> (Char, []), SCharLit l
      | StrLit l -> (Str, []), SStrLit l
      | Id var ->
        (type_of_identifier var symbols, unit_of_identifier var symbols), SId var
      | Paren e -> check_expr e
      | Assign (var, e) as ex ->
        let lt = type_of_identifier var symbols
        and lu = unit_of_identifier var symbols
        and (rt, ru), e' = check_expr e in
        let err =
          "illegal assignment "
          ^ string_of_typ lt
          ^ " = "
          ^ string_of_typ rt
          ^ string_of_unit_expr ru
          ^ " in "
          ^ string_of_expr ex
        in
        ( (check_type_assign lt rt err, check_unit_assign lu ru err)
        , SAssign (var, ((rt, ru), e')) )
      | Binop (e1, bop, e2) ->
        let (((t1, u1), e1') as se1) = check_expr e1
        and (((t2, u2), e2') as se2) = check_expr e2 in
        (* Determine expression type based on operator and operand types *)
        let check_intlit (e : sx) =
          match e with
          | SIntLit l -> l
          | _ -> raise (Failure (string_of_sexpr se2 ^ " is not int literal"))
        in
        let check_type_bop t1 t2 bop =
          match bop with
          | (Add | Sub | Mul) when t1 = t2 && t1 = Int -> Int
          | (Add | Sub | Mul) when t1 = t2 && t1 = Float -> Float
          | Pow when t1 = Int && t2 = Int && check_intlit e2' >= 0 -> Int
          | Pow when t1 = Float && t2 = Int && check_intlit e2' >= 0 -> Float
          | (Div | Mod) when t1 = t2 && t1 = Int -> Int
          | Div when t1 = t2 && t1 = Float -> Float
          | Equal | Neq -> Bool
          | (Geq | Leq | Great | Less) when t1 = Int || t1 = Float -> Bool
          | (And | Or) when t1 = Bool -> Bool
          | _ ->
            raise
              (Failure
                 ("illegal type in binary operator "
                 ^ string_of_typ t1
                 ^ " "
                 ^ string_of_bop bop
                 ^ " "
                 ^ string_of_typ t2))
        in
        (* Determine expression type based on operator and operand units *)
        let check_unit_bop u1 u2 bop =
          match bop with
          | Add | Sub -> unit_convert u1 u2
          | Mul -> unit_simplify (u1 @ u2)
          | Div -> unit_simplify (u1 @ List.map (fun (u, i) -> u, -i) u2)
          | Pow ->
            if u2 <> []
            then raise (Failure "exponent must be unitless")
            else (
              let rec repeat_unit (u : unit_expr) (n : int) =
                match n with
                | 1 -> u
                | _ when n > 1 -> u @ repeat_unit u (n - 1)
                | _ -> raise (Failure "n must be positive")
              in
              unit_simplify (repeat_unit u1 (check_intlit e2')))
          | Equal | Neq -> unit_convert u1 u2
          | Geq | Leq | Great | Less -> unit_convert u1 u2
          | _ -> u1 (* Do not check *)
        in
        let t = check_type_bop t1 t2 bop in
        (* TODO *)
        let u = check_unit_bop u1 u2 bop in
        (t, u), SBinop (((t1, u1), e1'), bop, ((t2, u2), e2'))
        (* else raise (Failure err) *)
      | Unaop (uop, e) as ex ->
        let (t, u), e' = check_expr e in
        let err =
          "illegal unary operator "
          ^ string_of_typ t
          ^ string_of_unit_expr u
          ^ " "
          ^ string_of_uop uop
          ^ " in "
          ^ string_of_expr ex
        in
        let t =
          match uop with
          | Neg when t = Int || t = Float -> t
          | Inc when t = Int || t = Float -> t
          | Dec when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure err)
        in
        let check_unit_uop u uop = u in
        let uu = check_unit_uop u uop in
        (t, uu), SUnaop (uop, ((t, u), e'))
      | Call (fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length
        then
          raise
            (Failure
               ("expecting "
               ^ string_of_int param_length
               ^ " arguments in "
               ^ string_of_expr call))
        else (
          let check_call (ft, _, fu) e =
            let (et, eu), e' = check_expr e in
            let err =
              "illegal argument found "
              ^ string_of_typ et
              ^ " "
              ^ string_of_unit_expr eu
              ^ " expected "
              ^ string_of_typ ft
              ^ " "
              ^ string_of_unit_expr fu
              ^ " in "
              ^ string_of_expr e
            in
            (check_type_assign ft et err, check_unit_assign fu eu err), e'
          in
          let args' = List.map2 check_call fd.formals args in
          fd.rtyp, SCall (fname, args'))
    in
    let check_bool_expr e =
      let (t, u), e' = check_expr e in
      match t with
      | Bool -> (t, u), e'
      | _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in
    (* let check_int_expr e =
         let ((t, u), e') = check_expr e in
         match t with
         | Int -> ((t, u), e')
         |  _ -> raise (Failure ("expected Int expression in " ^ string_of_expr e))
       in

       let check_assignable_expr e =
         let ((t, u), e') = check_expr e in
         match e' with
         | SId _ -> ((t, u), e')
         |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
       in *)
    let rec check_stmt_list = function
      | [] -> []
      | Block sl :: sl' -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt = function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> SBlock (check_stmt_list sl) (* A label treated as variable*)
      | LabelS (lb, st) -> check_stmt st
      | SimpleS st -> SSimpleS (check_simple_stmt st)
      | ReturnS e ->
        let el = List.map check_expr e in
        if List.length e = 0
        then SReturnS el
        else (
          let t = fst (List.hd el) in
          if t = func.rtyp
          then SReturnS el
          else
            raise
              (Failure
                 ("return gives "
                 ^ string_of_typ (fst t)
                 ^ string_of_unit_expr (snd t)
                 ^ " expected "
                 ^ string_of_rtyp func.rtyp
                 ^ " in "
                 ^ string_of_stmt (ReturnS e))))
      | IfS (simple, expr, stmt1, stmt2) ->
        let sim =
          match simple with
          | None -> None
          | Some v -> Some (check_simple_stmt v)
        in
        let e = check_bool_expr expr in
        let st1 = check_stmt stmt1 in
        let st2 =
          match stmt2 with
          | None -> None
          | Some v -> Some (check_stmt v)
        in
        SIfS (sim, e, st1, st2)
      | SwitchS (simple, expr, casel) ->
        let sim =
          match simple with
          | None -> None
          | Some v -> Some (check_simple_stmt v)
        in
        let e =
          match expr with
          | None -> None
          | Some v -> Some (check_expr v)
        in
        let sc =
          List.map
            (fun case ->
              match case with
              | CaseS (el, sl) ->
                let el' = List.map check_expr el in
                let sl' = List.map check_stmt sl in
                SCaseS (el', sl'))
            casel
        in
        SSwitchS (sim, e, sc)
      | MatchS (simple, var, expr, matchl) ->
        let sim =
          match simple with
          | None -> None
          | Some v -> Some (check_simple_stmt v)
        in
        (* add var to symbol table ? *)
        let e = check_expr expr in
        let mc =
          List.map
            (fun case ->
              match case with
              | MatchC (t, sl) ->
                let sl' = List.map check_stmt sl in
                SMatchC (t, sl'))
            matchl
        in
        SMatchS (sim, var, e, mc)
      | ForS (ftype, stmt) ->
        let f =
          match ftype with
          | Condition c -> SCondition (check_expr c)
          | FClause (stmt, expr, sstmt) ->
            let s =
              match stmt with
              | None -> None
              | Some v -> Some (check_stmt v)
            in
            let e =
              match expr with
              | None -> None
              | Some v -> Some (check_expr v)
            in
            let ss =
              match sstmt with
              | None -> None
              | Some v -> Some (check_simple_stmt v)
            in
            SFClause (s, e, ss)
          | RClause (id, expr) -> SRClause (id, check_expr expr)
        in
        SForS (f, check_stmt stmt)
      | LoopS ctrl ->
        let l =
          match ctrl with
          | BreakS b -> SBreakS b
          | ContinueS c -> SContinueS c
        in
        SLoopS l
      | FallS _ -> SFallS 0
    and check_simple_stmt = function
      | ExprS e -> SExprS (check_expr e)
    in
    (* body of check_func *)
    { srtyp = func.rtyp
    ; sfname = func.fname
    ; sformals = func.formals
    ; slocals = func.locals
    ; sbody = check_stmt_list func.body
    }
  in
  let check_unit unt =
    let rec check_num_expr = function
      | IntLit (l, u) -> (Int, u), SIntLit l
      | BoolLit l -> (Bool, []), SBoolLit l
      | FloatLit (l, u) -> (Float, []), SFloatLit l
      | CharLit l -> (Char, []), SCharLit l
      | StrLit l -> (Str, []), SStrLit l
      | Id var ->
        (type_of_identifier var global_vars, unit_of_identifier var global_vars), SId var
      | Binop (e1, bop, e2) as e ->
        let (t1, u1), e1' = check_num_expr e1
        and (t2, u2), e2' = check_num_expr e2 in
        let err =
          "illegal binary operator "
          ^ string_of_typ t1
          ^ string_of_unit_expr u1
          ^ " "
          ^ string_of_bop bop
          ^ " "
          ^ string_of_typ t2
          ^ string_of_unit_expr u2
          ^ " in "
          ^ string_of_expr e
        in
        (* All binary operators require operands of the same type except pow *)
        (* if t1 = t2 then *)
        (* Determine expression type based on operator and operand types *)
        let t =
          match bop with
          | (Add | Sub | Mul) when t1 = t2 && t1 = Int -> Int
          | (Add | Sub | Mul) when t1 = t2 && t1 = Float -> Float
          | Pow when t1 = Int && t2 = Int -> Int
          | Pow when t1 = Float && t2 = Int -> Float
          | (Div | Mod) when t1 = t2 && t1 = Int -> Int
          | Div when t1 = t2 && t1 = Float -> Float
          | Equal | Neq -> Bool
          | (Geq | Leq | Great | Less) when t1 = Int || t1 = Float -> Bool
          | (And | Or) when t1 = Bool -> Bool
          | _ -> raise (Failure err)
        in
        (* TODO *)
        let check_unit_bop u1 u2 bop = u1 in
        (t, check_unit_bop u1 u2 bop), SBinop (((t1, u1), e1'), bop, ((t2, u2), e2'))
      | Call (fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length
        then
          raise
            (Failure
               ("expecting "
               ^ string_of_int param_length
               ^ " arguments in "
               ^ string_of_expr call))
        else (
          let check_call (ft, _, fu) e =
            let (et, eu), e' = check_num_expr e in
            let err =
              "illegal argument found "
              ^ string_of_typ et
              ^ " "
              ^ string_of_unit_expr eu
              ^ " expected "
              ^ string_of_typ ft
              ^ " "
              ^ string_of_unit_expr fu
              ^ " in "
              ^ string_of_expr e
            in
            (check_type_assign ft et err, check_unit_assign fu eu err), e'
          in
          let args' = List.map2 check_call fd.formals args in
          fd.rtyp, SCall (fname, args'))
      | _ as l ->
        raise (Failure ("Invalid operation for unit declaration: " ^ string_of_expr l))
    in
    match snd unt with
    | BaseUnit -> fst unt, SBaseUnit
    | AUnit l -> fst unt, SAUnit l
    | CUnit (e, id) -> fst unt, SCUnit (check_num_expr e, id)
  in
  let check_utype = function
    | VarType (name, type_list) -> SVarType (name, type_list)
    | StructType (name, bind_list) -> SStructType (name, bind_list)
    | TensorType (name, shape_list) -> STensorType (name, shape_list)
    | ArrType (name, shape_list) -> SArrType (name, shape_list)
  in
  ( globals
  , List.map check_unit units
  , List.map check_utype utypes
  , List.map check_func functions )
;;
