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
  let check_binds
      (kind : string)
      (binds : (typ * int list * string * unit_expr * expr option) list)
    =
    let rec dups = function
      | [] -> ()
      | (_, _, n1, _, _) :: (_, _, n2, _, _) :: _ when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in
    dups (List.sort (fun (_, _, a, _, _) (_, _, b, _, _) -> compare a b) binds)
  in
  (* Make sure no globals duplicate *)
  let _ = check_binds "global" globals in
  (* Create user type table *)
  let add_type tuple = function
    | VarType (name, type_list) -> StringMap.add name type_list (fst tuple), snd tuple
    | StructType (name, binds) -> fst tuple, StringMap.add name binds (snd tuple)
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
  (* whether typ t is contained in vartype vt *)
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
    | (UserType type_name as ty), sh, name, uexpr, _ ->
      check_usertyped_var type_name;
      StringMap.add name (ty, uexpr, sh) m
    | ty, sh, name, uexpr, _ -> StringMap.add name (ty, uexpr, sh) m
  in
  (* global variables *)
  (* let global_vars = List.fold_left add_var StringMap.empty globals in *)
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
    try
      match StringMap.find s m with
      | t, _, _ -> t
    with
    | Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  let is_correct_field_in_bind target = function
    | _, _, src, _, _ when src = target -> true
    | _ -> false
  in
  let bind_of_struct_field s f m =
    let binds =
      try StringMap.find s m with
      | Not_found -> raise (Failure ("undeclared struct " ^ s))
    in
    try List.find (is_correct_field_in_bind f) binds with
    | Not_found -> raise (Failure ("undeclared field " ^ f ^ "in struct " ^ s))
  in
  let struct_of_var v m =
    match
      try
        match StringMap.find v m with
        | t, _, _ -> t
      with
      | Not_found -> raise (Failure ("undeclared identifier " ^ v))
    with
    | UserType s -> s
    | _ -> raise (Failure ("not struct " ^ v))
  in
  let type_of_var_field var f var_m struct_m =
    let s = struct_of_var var var_m in
    let b = bind_of_struct_field s f struct_m in
    match b with
    | t, _, _, _, _ -> t
  in
  let unit_of_var_field var f var_m struct_m =
    let s = struct_of_var var var_m in
    let b = bind_of_struct_field s f struct_m in
    match b with
    | _, _, _, u, _ -> u
  in
  (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
  let check_type_assign lvaluet rvaluet err =
    if lvaluet = UserType "Any"
    then rvaluet
    else if lvaluet = rvaluet || (is_vartype lvaluet && has_subtype lvaluet rvaluet)
    then lvaluet
    else raise (Failure err)
  in
  let unit_of_identifier s m =
    try
      match StringMap.find s m with
      | _, u, _ -> u
    with
    | Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  (* check if unit defined *)
  let check_unit_expr (uexpr : unit_expr) =
    let f checked (u, exp) =
      if StringMap.mem u global_units
      then (u, exp) :: checked
      else raise (Failure ("unit " ^ u ^ " not defined"))
    in
    List.rev (List.fold_left f [] uexpr)
  in
  (* Check if the given rvalue unit expr can be assigned to the given lvalue unit expr *)
  let check_unit_assign lu ru err =
    (* if lvaluet = rvaluet then lvaluet else raise (Failure err) *)
    if lu = ru || ru = []
    then lu
    else
      raise
        (Failure
           ("Incompatible unit: "
           ^ string_of_unit_expr lu
           ^ " <- "
           ^ string_of_unit_expr ru))
  in
  (* try to convert the rhs unit to the lhs unit *)
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
      | _ -> raise (Failure ("cannot convert unit " ^ u2 ^ " to " ^ u1)))
  in
  (* try to convert the rhs unit term to the lhs unit term *)
  let unit_term_convert (u1, i1) (u2, i2) =
    (* power must equal *)
    if i1 = i2
    then (
      match unit_convert u1 u2, i1 with
      | One, _ -> One
      | MulE e, i -> if i = 1 then MulE e else MulE (Binop (e, Pow, IntLit (i1, [])))
      | DivE e, i -> if i = 1 then DivE e else DivE (Binop (e, Pow, IntLit (i1, []))))
    else
      raise
        (Failure
           ("cannot convert unit term "
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
  (* split unit expr into power 1 term and sort them by their base unit to facilitate later convert *)
  let rec flatten_unit_expr (ue : unit_expr) =
    let to_key (u : string) : string =
      let base = StringMap.find u global_units in
      match base with
      | BaseUnit -> u
      | CUnit (_, b) -> b
      | _ -> raise (Failure "Abstract unit is not supported")
    in
    let ue_sorted =
      List.sort (fun ut1 ut2 -> compare (to_key (fst ut1)) (to_key (fst ut2))) ue
    in
    match ue_sorted with
    | [] -> []
    | hd :: tl -> repeat (fst hd, 1) (snd hd) @ flatten_unit_expr tl
  in
  (* try to convert the rhs unit expr to the lhs unit expr *)
  let unit_expr_convert ue1 ue2 =
    if ue1 = ue2 || ue2 = [] || ue1 = []
    then []
    else (
      let ue1' = flatten_unit_expr ue1 in
      let ue2' = flatten_unit_expr ue2 in
      if List.length ue1' != List.length ue2'
      then
        raise
          (Failure
             ("cannot convert unit expr "
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
  (* try to update the expr so that its unit expr is changed from rhs to lhs *)
  let convert_expr_by_unit expr ue1 ue2 =
    let f expr convE =
      match convE with
      | One -> expr
      | MulE e -> Binop (expr, Mul, e)
      | DivE e -> Binop (expr, Div, e)
    in
    List.fold_left f expr (unit_expr_convert ue1 ue2)
  in
  (* combine adjacent unit term if same unit *)
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
      ; formals = [ UserType "Any", [], "x", [], None ]
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
  let rec check_expr (symbols : (typ * unit_expr * int list) StringMap.t) = function
    | IntLit (l, u) -> (Int, check_unit_expr u), SIntLit l
    | BoolLit l -> (Bool, []), SBoolLit l
    | FloatLit (l, u) -> (Float, check_unit_expr u), SFloatLit l
    | CharLit l -> (Char, []), SCharLit l
    | StrLit l -> (Str, []), SStrLit l
    | Id var ->
      ( (type_of_identifier var symbols, check_unit_expr (unit_of_identifier var symbols))
      , SId var )
    | StructLit (name, el) as slit ->
      let binds = StringMap.find name global_structs in
      if List.length binds != List.length el
      then
        raise
          (Failure
             ("expecting "
             ^ string_of_int (List.length binds)
             ^ " expressions in "
             ^ string_of_expr slit))
      else (
        let check_structlit (ft, _, _, fu, _) e =
          let (et, eu), e' = check_expr symbols e in
          let _, e' = check_expr symbols (convert_expr_by_unit e fu eu) in
          let eu = fu in
          let err =
            "illegal field assignment "
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
        let el' = List.map2 check_structlit binds el in
        (UserType name, []), SStructLit (name, el'))
    | FieldLit (v, f) ->
      ( ( type_of_var_field v f symbols global_structs
        , unit_of_var_field v f symbols global_structs )
      , SFieldLit (v, f) )
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
    | AssignField (var, f, e) as ex ->
      let lt = type_of_var_field var f symbols global_structs
      and lu = unit_of_var_field var f symbols global_structs
      and (rt, ru), e' = check_expr symbols e in
      let _, e' = check_expr symbols (convert_expr_by_unit e lu ru) in
      let ru = lu in
      let err =
        "illegal assignment "
        ^ string_of_typ lt
        ^ string_of_unit_expr lu
        ^ " = "
        ^ string_of_typ rt
        ^ string_of_unit_expr ru
        ^ " in "
        ^ string_of_expr ex
      in
      ( (check_type_assign lt rt err, check_unit_assign lu ru err)
      , SAssignField (var, f, ((rt, ru), e')) )
    | Binop (e1, bop, e2) ->
      let (((t1, u1), e1') as se1) = check_expr symbols e1
      and (((t2, u2), e2') as se2) = check_expr symbols e2 in
      (* Determine expression type based on operator and operand types *)
      let check_intlit (e : sx) =
        match e with
        | SIntLit l -> l
        | _ -> raise (Failure (string_of_sexpr se2 ^ " is not int literal"))
      in
      let err_msg =
        "illegal type in binary operator "
        ^ string_of_typ t1
        ^ " "
        ^ string_of_bop bop
        ^ " "
        ^ string_of_typ t2
      in
      let check_type_bop t1 t2 bop =
        match bop with
        | (Add | Sub | Mul) when t1 = t2 && t1 = Int -> Int
        | (Add | Sub | Mul) when t1 = t2 && t1 = Float -> Float
        | Pow when t1 = Int && t2 = Int ->
          if check_intlit e2' > 0
          then Int
          else raise (Failure (err_msg ^ " (exponent in power op must be positive"))
        | Pow when t1 = Float && t2 = Int ->
          if check_intlit e2' > 0
          then Float
          else raise (Failure (err_msg ^ " (exponent in power op must be positive"))
        | (Div | Mod) when t1 = t2 && t1 = Int -> Int
        | Div when t1 = t2 && t1 = Float -> Float
        | Equal | Neq -> Bool
        | (Geq | Leq | Great | Less) when t1 = Int || t1 = Float -> Bool
        | (And | Or) when t1 = Bool -> Bool
        | _ -> raise (Failure err_msg)
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
        | Not when t = Bool -> Bool
        | _ -> raise (Failure err)
      in
      let check_unit_uop u uop = u in
      let uu = check_unit_uop u uop in
      (t, uu), SUnaop (uop, ((t, u), e'))
    | IoDop (e, iodop) as ex ->
      let (t, u), e' = check_expr symbols e in
      let err =
        "illegal type for inc or dec operator "
        ^ string_of_typ t
        ^ string_of_unit_expr u
        ^ " "
        ^ string_of_iodop iodop
        ^ " in "
        ^ string_of_expr ex
      in
      let t =
        match iodop with
        | (Inc | Dec) when t = Int -> t
        | _ -> raise (Failure err)
      in
      let check_unit_uop u iodop = u in
      let uu = check_unit_uop u iodop in
      (t, uu), SIodop (((t, u), e'), iodop)
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
        let check_call (ft, _, _, fu, _) e =
          let (et, eu), e' = check_expr symbols e in
          let _, e' = check_expr symbols (convert_expr_by_unit e fu eu) in
          let eu = fu in
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
  (* let check_bind_init symbols (t,v,u,e) =
       let e' = check_expr e
       in (t,v,u,e')
     in *)
  let check_binds_init symbols (binds : bind list) =
    let f (symbols, sbinds) ((t, sh, v, u, e) : bind) =
      let e' =
        match e with
        | Some ee -> Some (check_expr symbols ee)
        | None -> None
      in
      let sbind = t, sh, v, u, e' in
      StringMap.add v (t, u, sh) symbols, sbind :: sbinds
    in
    let symbols, sbinds = List.fold_left f (symbols, []) binds in
    symbols, List.rev sbinds
  in
  let global_vars, sglobals = check_binds_init StringMap.empty globals in
  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;
    (* turn formals to sformals *)
    let sformals = List.map (fun (t, sh, v, u, _) -> t, sh, v, u, None) func.formals in
    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left add_var StringMap.empty (globals @ func.formals) in
    let symbols, slocals = check_binds_init symbols func.locals in
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
        let default_count =
          List.fold_left
            (fun count case ->
              match case with
              | CaseS (el, sl) -> if List.length el == 0 then count + 1 else count)
            0
            casel
        in
        if default_count > 1 then raise (Failure "Multiple default labels.");
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
          (* check last case is default *)
          let rec check_last matchl =
            match matchl with
            | [] -> ()
            | [ hd ] ->
              (match hd with
              | MatchC (Some t, _) -> raise (Failure "Last case in match must be default")
              | MatchC (None, _) -> ())
            | hd :: tl -> check_last tl
          in
          check_last matchl;
          let mc =
            List.map
              (fun case ->
                match case with
                | MatchC (Some t, sl) ->
                  ignore (check_subtyp vt t);
                  let sl' =
                    check_stmt_list (add_var symbols (t, [], var, ue, None)) c_ctrl sl
                  in
                  SMatchC (Some t, sl')
                | MatchC (None, sl) ->
                  let sl' =
                    check_stmt_list (add_var symbols (vt, [], var, ue, None)) c_ctrl sl
                  in
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
    ; sformals
    ; slocals
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
  let check_fields (binds : (typ * int list * string * unit_expr * expr option) list)
      : sbind list
    =
    let rec dups = function
      | [] -> ()
      | (_, _, n1, _, _) :: (_, _, n2, _, _) :: _ when n1 = n2 ->
        raise (Failure ("duplicate field" ^ " " ^ n1))
      | _ :: t -> dups t
    in
    let f ((t, sh, v, u, e) : bind) : sbind =
      let e' =
        match e with
        | Some ee ->
          raise (Failure "Initialize the field with expression is not supported yet!")
          (* TODO *)
        | None -> None
      in
      t, sh, v, u, e'
    in
    let sbinds = List.map f binds in
    dups (List.sort (fun (_, _, a, _, _) (_, _, b, _, _) -> compare a b) binds);
    sbinds
  in
  let check_utype = function
    | VarType (name, type_list) -> SVarType (name, type_list)
    | StructType (name, binds) ->
      let sbinds = check_fields binds in
      SStructType (name, sbinds)
  in
  ( sglobals
  , List.map check_unit_decl units
  , List.map check_utype utypes
  , List.map check_func functions )
;;
