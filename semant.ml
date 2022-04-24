(* Semantic checking for the MicroC compiler *)

open Ast
open Sast
module StringMap = Map.Make (String)

type control =
  { can_break : bool
  ; can_continue : bool
  ; can_fall : bool
  }

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

type coef =
  | One
  | MulE of expr
  | DivE of expr

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
  (* Create global variable symbol table *)
  let global_vartypes = fst global_type_tuple in
  let global_structs = snd global_type_tuple in
  let is_vartype = function
    | UserType type_name -> StringMap.mem type_name global_vartypes
    | _ -> false
  in
  let has_subtype vt t =
    let typ_list = StringMap.find (string_of_typ vt) global_vartypes in
    List.mem t typ_list
  in
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
  (* global variables *)
  let global_vars = List.fold_left add_var StringMap.empty globals in
  (* global unit table *)
  let global_units = StringMap.empty in
  let global_units = StringMap.add "s" BaseUnit global_units in
  let global_units = StringMap.add "m" BaseUnit global_units in
  let global_units = StringMap.add "kg" BaseUnit global_units in
  let global_units = StringMap.add "A" BaseUnit global_units in
  let global_units = StringMap.add "K" BaseUnit global_units in
  let global_units = StringMap.add "mol" BaseUnit global_units in
  let global_units = StringMap.add "cd" BaseUnit global_units in
  let add_unit m unt = StringMap.add (fst unt) (snd unt) m in
  let global_units = List.fold_left add_unit global_units units in
  let type_of_identifier s m =
    try fst (StringMap.find s m) with
    | Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  let unit_of_identifier s m =
    try snd (StringMap.find s m) with
    | Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  let check_unit_expr (uexpr : unit_expr) =
    let f checked (u, exp) =
      if StringMap.mem u global_units
      then (u, exp) :: checked
      else raise (Failure ("unit " ^ u ^ " not defined"))
    in
    List.rev (List.fold_left f [] uexpr)
  in
  (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
  let check_type_assign lvaluet rvaluet err =
    if lvaluet = rvaluet || (is_vartype lvaluet && has_subtype lvaluet rvaluet)
    then lvaluet
    else raise (Failure err)
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
    if u1 = u2
    then One
    else (
      let p1 = StringMap.find u1 global_units in
      let p2 = StringMap.find u2 global_units in
      match p1, p2 with
      | BaseUnit, CUnit (e, id) when u1 = id -> MulE e
      | CUnit (e, id), BaseUnit when u2 = id -> DivE e
      | CUnit (e1, id1), CUnit (e2, id2) when id1 = id2 -> MulE (Binop (e2, Div, e1))
      | _ -> raise (Failure ("cannot convert " ^ u2 ^ " to " ^ u1)))
  in
  let unit_term_convert (u1, i1) (u2, i2) =
    if i1 = i2
    then (
      match unit_convert u1 u2, i1 with
      | One, _ -> One
      | MulE e, i -> if i = 1 then MulE e else MulE (Binop (e, Pow, IntLit (i1, [])))
      | DivE e, i -> if i = 1 then DivE e else DivE (Binop (e, Pow, IntLit (i1, []))))
    else
      raise
        (Failure
           ("cannot convert "
           ^ string_of_unit_term (u1, i1)
           ^ " to "
           ^ string_of_unit_term (u2, i2)))
  in
  let rec repeat item n =
    match n with
    | 1 | -1 -> [ item ]
    | _ when n > 1 -> item :: repeat item (n - 1)
    | _ when n < 1 -> item :: repeat item (n + 1)
    | _ -> raise (Failure "n must be non-zero")
  in
  let rec flatten_unit_expr = function
    | [] -> []
    | hd :: tl -> repeat (fst hd, 1) (snd hd) @ flatten_unit_expr tl
  in
  let unit_expr_convert ue1 ue2 =
    if ue1 = ue2 || ue2 = []
    then []
    else (
      let ue1' = flatten_unit_expr ue1 in
      let ue2' = flatten_unit_expr ue2 in
      if List.length ue1' != List.length ue2'
      then
        raise
          (Failure
             ("cannot convert "
             ^ string_of_unit_expr ue1
             ^ " to "
             ^ string_of_unit_expr ue2))
      else (
        let f convE ut1 ut2 =
          let newE = unit_term_convert ut1 ut2 in
          newE :: convE
        in
        List.rev (List.fold_left2 f [] ue1' ue2')))
  in
  let convert_expr_by_unit expr ue1 ue2 =
    let f expr convE =
      match convE with
      | One -> expr
      | MulE e -> Binop (expr, Mul, e)
      | DivE e -> Binop (expr, Div, e)
    in
    List.fold_left f expr (unit_expr_convert ue1 ue2)
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
  let rec check_expr symbols = function
    | IntLit (l, u) -> (Int, check_unit_expr u), SIntLit l
    | BoolLit l -> (Bool, []), SBoolLit l
    | FloatLit (l, u) -> (Float, check_unit_expr u), SFloatLit l
    | CharLit l -> (Char, []), SCharLit l
    | StrLit l -> (Str, []), SStrLit l
    | Id var ->
      ( (type_of_identifier var symbols, check_unit_expr (unit_of_identifier var symbols))
      , SId var )
    | Paren e -> check_expr symbols e
    | Assign (var, e) as ex ->
      let lt = type_of_identifier var symbols
      and lu = unit_of_identifier var symbols
      and (rt, ru), e' = check_expr symbols e in
      let _, e' = check_expr symbols (convert_expr_by_unit e lu ru) in
      let ru = lu in
      let err =
        "illegal assignment "
        ^ string_of_typ lt
        ^ " <- "
        ^ string_of_typ rt
        ^ string_of_unit_expr ru
        ^ " in "
        ^ string_of_expr ex
      in
      ( (check_type_assign lt rt err, check_unit_assign lu ru err)
      , SAssign (var, ((rt, ru), e')) )
    | Binop (e1, bop, e2) ->
      let (((t1, u1), e1') as se1) = check_expr symbols e1
      and (((t2, u2), e2') as se2) = check_expr symbols e2 in
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
        | Add | Sub -> u1
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
        | Equal | Neq -> u1
        | Geq | Leq | Great | Less -> u1
        | _ -> u1
        (* Do not check *)
      in
      let t = check_type_bop t1 t2 bop in
      (* TODO *)
      let u = check_unit_bop u1 u2 bop in
      (match bop with
      | Add | Sub | Equal | Neq | Geq | Leq | Great | Less ->
        ( (t, u)
        , SBinop
            ( ((t1, u1), e1')
            , bop
            , ((t2, u1), snd (check_expr symbols (convert_expr_by_unit e2 u1 u2))) ) )
      | _ -> (t, u), SBinop (((t1, u1), e1'), bop, ((t2, u2), e2')))
      (* (t, u), SBinop (((t1, u1), e1'), bop, ((t2, u2), e2')) *)
      (* else raise (Failure err) *)
    | Unaop (uop, e) as ex ->
      let (t, u), e' = check_expr symbols e in
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
          let (et, eu), e' = check_expr symbols e in
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
    let check_bool_expr e =
      let (t, u), e' = check_expr symbols e in
      match t with
      | Bool -> (t, u), e'
      | _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in
    let rec check_stmt_list symbols ctrl = function
      | [] -> []
      | FallS f :: s :: sl ->
        ignore (check_stmt symbols ctrl (FallS f));
        raise (Failure "fallthough must be the last statement")
      (* 
      'break' will generate 'br END' and ignore all the remaining codes in the same block
      'continue' will generate 'br Cond' or 'br Update' and ignore all the remaining codes in the same block
      *)
      | LoopS loo :: sl ->
        let l =
          match loo with
          | BreakS b -> check_stmt symbols ctrl (LoopS loo)
          | ContinueS c -> check_stmt symbols ctrl (LoopS loo)
        in
        [ l ]
      | Block sl :: sl' -> check_stmt_list symbols ctrl (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt symbols ctrl s :: check_stmt_list symbols ctrl sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt symbols ctrl = function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> SBlock (check_stmt_list symbols ctrl sl)
      | LabelS (lb, st) -> check_stmt symbols ctrl st
      | ExprS e -> SExprS (check_expr symbols e)
      | ReturnS e ->
        let el = List.map (check_expr symbols) e in
        if List.length e = 0
        then SReturnS el
        else (
          (* Fix: each funciton has exactly one return value *)
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
          | Some v -> Some (check_expr symbols v)
        in
        let e = check_bool_expr expr in
        let st1 = check_stmt symbols ctrl stmt1 in
        let st2 =
          match stmt2 with
          | None -> None
          | Some v -> Some (check_stmt symbols ctrl v)
        in
        SIfS (sim, e, st1, st2)
      | SwitchS (simple, expr, casel) ->
        let sim =
          match simple with
          | None -> None
          | Some v -> Some (check_expr symbols v)
        in
        let e =
          match expr with
          | None -> None
          | Some v -> Some (check_expr symbols v)
        in
        let s_ctrl =
          { can_break = true; can_continue = ctrl.can_continue; can_fall = true }
        in
        let sc =
          List.map
            (fun case ->
              match case with
              | CaseS (el, sl) ->
                let el' = List.map (check_expr symbols) el in
                let sl' = check_stmt_list symbols s_ctrl sl in
                SCaseS (el', sl'))
            casel
        in
        SSwitchS (sim, e, sc)
      | MatchS (simple, var, expr, matchl) ->
        let sim =
          match simple with
          | None -> None
          | Some v -> Some (check_expr symbols v)
        in
        (* add var to symbol table ? *)
        let (((vt, ue), expr') as ce) = check_expr symbols expr in
        if not (is_vartype vt)
        then raise (Failure "match rhs expr must be a vartype variable")
        else (
          let c_ctrl =
            { can_break = true; can_continue = ctrl.can_continue; can_fall = true }
          in
          let check_subtyp vt t =
            if not (has_subtype vt t)
            then
              raise
                (Failure (string_of_typ t ^ " is not included in " ^ string_of_typ vt))
          in
          let mc =
            List.map
              (fun case ->
                match case with
                | MatchC (Some t, sl) ->
                  ignore (check_subtyp vt t);
                  let sl' = check_stmt_list (add_var symbols (t, var, ue)) c_ctrl sl in
                  SMatchC (Some t, sl')
                | MatchC (None, sl) ->
                  let sl' = check_stmt_list (add_var symbols (vt, var, ue)) c_ctrl sl in
                  SMatchC (None, sl'))
              matchl
          in
          SMatchS (sim, var, ce, mc))
      | ForS (ftype, stmt) ->
        let f_ctrl =
          { can_break = true; can_continue = true; can_fall = ctrl.can_fall }
        in
        let f =
          match ftype with
          | Condition c -> SCondition (check_expr symbols c)
          | FClause (stmt, expr, sstmt) ->
            let s =
              match stmt with
              | None -> None
              | Some v -> Some (check_stmt symbols f_ctrl v)
            in
            let e =
              match expr with
              | None -> None
              | Some v -> Some (check_expr symbols v)
            in
            let ss =
              match sstmt with
              | None -> None
              | Some v -> Some (check_expr symbols v)
            in
            SFClause (s, e, ss)
          | RClause (id, expr) -> SRClause (id, check_expr symbols expr)
        in
        SForS (f, check_stmt symbols f_ctrl stmt)
      | LoopS loo ->
        let l =
          match loo with
          | BreakS b ->
            if ctrl.can_break
            then SBreakS b
            else raise (Failure "break not in for/switch/match")
          | ContinueS c ->
            if ctrl.can_continue
            then SContinueS c
            else raise (Failure "continue not in for statment")
        in
        SLoopS l
      | FallS _ ->
        if ctrl.can_fall
        then SFallS 0
        else raise (Failure "fallthrough not in switch/match statment")
      (* and check_simple_stmt = function
      | ExprS e -> SExprS (check_expr e) *)
    in
    (* body of check_func *)
    { srtyp = func.rtyp
    ; sfname = func.fname
    ; sformals = func.formals
    ; slocals = func.locals
    ; sbody =
        check_stmt_list
          symbols
          { can_break = false; can_continue = false; can_fall = false }
          func.body
    }
  in
  let check_unit_decl unt =
    let check_num_expr e =
      let ckd = check_expr global_vars e in
      match ckd with
      | (Int, []), _ -> ckd
      | (Float, []), _ -> ckd
      | _ ->
        raise (Failure ("Invalid operation for unit declaration: " ^ string_of_sexpr ckd))
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
  , List.map check_unit_decl units
  , List.map check_utype utypes
  , List.map check_func functions )
;;
