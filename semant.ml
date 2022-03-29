(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check ((globals, units, vtypes, functions):program) =

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (typ * string * unit_expr) list) =
    let rec dups = function
        [] -> ()
      |	((_,n1,_) :: (_,n2,_) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a,_) (_,b,_) -> compare a b) binds)
  in

  (* Make sure no globals duplicate *)
  let _ = check_binds "global" globals in

  (* Create global variable symbol table *)
  let global_vars = List.fold_left (fun m (ty, name, uexpr) -> StringMap.add name (ty,uexpr) m) StringMap.empty globals in

  let type_of_identifier s m =
    try fst (StringMap.find s m)
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  let unit_of_identifier s m =
    try snd (StringMap.find s m)
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
  let check_type_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in

  let check_unit_assign lu ru err =
    (* if lvaluet = rvaluet then lvaluet else raise (Failure err) *)
    lu
  in

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    StringMap.add "print" {
      rtyp = (Int, []);
      fname = "print";
      formals = [(Int, "x", [])];
      locals = []; body = [] } StringMap.empty
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;


    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name, uexpr) -> StringMap.add name (ty, uexpr) m)
        StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = function
        IntLit (l, u) -> ((Int, u), SIntLit l)
      | BoolLit l -> ((Bool, []), SBoolLit l)
      | FloatLit (l, u) -> ((Float, u), SFloatLit l)
      | CharLit l -> ((Char, []), SCharLit l)
      | StrLit l -> ((Str, []), SStrLit l)
      | Id var -> ((type_of_identifier var symbols, unit_of_identifier var symbols), SId var)
      | Paren(e) -> check_expr e
      | Assign(var, e) as ex ->
        let lt = type_of_identifier var symbols
        and lu = unit_of_identifier var symbols
        and ((rt, ru), e') = check_expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ string_of_unit_expr ru ^ " in " ^ string_of_expr ex
        in
        ((check_type_assign lt rt err, check_unit_assign lu ru err), SAssign(var, ((rt, ru), e')))

      | Binop(e1, bop, e2) as e ->
        let ((t1, u1), e1') = check_expr e1
        and ((t2, u2), e2') = check_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ string_of_unit_expr u1 ^ " " ^ string_of_bop bop ^ " " ^
                  string_of_typ t2 ^ string_of_unit_expr u2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match bop with
              Add | Sub | Mul when t1 = Int -> Int
            | Add | Sub | Mul when t1 = Float -> Float
            | Div | Mod when t1 = Int   -> if e2' = SIntLit(0)
              then raise(Failure("Div by 0: " ^ string_of_expr e)) else Int
            | Div when t1 = Float -> if e2' = SFloatLit(0.)
              then raise(Failure("Div by 0.0: " ^ string_of_expr e)) else Float
            | Equal | Neq -> Bool 
            | Geq | Leq | Great | Less when t1 = Int || t1 = Float -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (* TODO *)
          let check_unit_bop u1 u2 bop = u1 in
          ((t, check_unit_bop u1 u2 bop), SBinop(((t1, u1), e1'), bop, ((t2, u2), e2')))
        else raise (Failure err)

      | Unaop(uop, e) as ex ->
        let ((t, u), e') = check_expr e in 
        let err = "illegal unary operator " ^ 
            string_of_typ t ^ string_of_unit_expr u ^ " " ^ string_of_uop uop ^ " in " ^ string_of_expr ex
        in 
        let t = match uop with
              Neg when t = Int || t = Float -> t
            | Inc when t = Int || t = Float -> t
            | Dec when t = Int || t = Float -> t
            | Not when t = Bool -> Bool
            | _ -> raise (Failure err)
        in let check_unit_uop uop u = u in
        ((t, check_unit_uop uop u), SUnaop(uop, ((t, u), e')))
      
      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _, fu) e =
               let ((et, eu), e') = check_expr e in
               let err = "illegal argument found " ^ string_of_typ et ^ " " ^ string_of_unit_expr eu ^
                         " expected " ^ string_of_typ ft ^ " " ^ string_of_unit_expr fu ^ " in " ^ string_of_expr e
               in ((check_type_assign ft et err, check_unit_assign fu eu err), e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
    in

    let check_bool_expr e =
      let ((t, u), e') = check_expr e in
      match t with
      | Bool -> ((t, u), e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
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

    let rec check_stmt_list =function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt =function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list sl)
        (* A label treated as variable*)
      | LabelS(lb, st) ->  check_stmt st
      | SimpleS st -> SSimpleS(check_simple_stmt st)
      | ReturnS e -> 
        let el = List.map check_expr e in
        if (List.length e) = 0 then SReturnS el else
        let t = fst (List.hd el) in 
        if t = func.rtyp then SReturnS el
        else raise (
            Failure ("return gives " ^ string_of_typ (fst t) ^ string_of_unit_expr (snd t) ^ " expected " ^
                     string_of_rtyp func.rtyp ^ " in " ^ string_of_stmt (ReturnS(e)) ))
      | IfS(simple, expr, stmt1, stmt2) -> 
        let sim = match simple with None -> None | Some v -> Some (check_simple_stmt v) in 
        let e = check_bool_expr expr in 
        let st1 = check_stmt stmt1 in 
        let st2 = match stmt2 with None -> None | Some v -> Some (check_stmt v) in 
        SIfS(sim, e, st1, st2)
      | SwitchS(simple, expr, casel) -> 
        let sim = match simple with None -> None | Some v -> Some (check_simple_stmt v) in 
        let e = match expr with None -> None | Some v -> Some (check_expr v) in 
        let sc = List.map (
          fun case -> 
            match case with
            CaseS(el, sl) ->
          let el' = List.map check_expr el in 
          let sl' = List.map check_stmt sl in 
          SCaseS(el', sl')) casel in 
        SSwitchS(sim, e, sc)
      | MatchS(simple, var, expr, matchl) -> 
        let sim = match simple with None -> None | Some v -> Some (check_simple_stmt v) in
        (* add var to symbol table ? *)
        let e = check_expr expr in 
        let mc = List.map (
          fun case ->
            match case with
            MatchC(t, sl) ->
          let sl' = List.map check_stmt sl in 
          SMatchC(t, sl')) matchl in 
        SMatchS(sim, var, e, mc)
      | ForS(ftype, stmt) ->
        let f =
          begin
          match ftype with
          | Condition(c) -> SCondition(check_expr c)
          | FClause(stmt, expr, sstmt) -> 
            let s = match stmt with None -> None | Some v -> Some (check_stmt v) in
            let e = match expr with None -> None | Some v -> Some (check_expr v) in
            let ss = match sstmt with None -> None | Some v -> Some (check_simple_stmt v) in
            SFClause(s, e, ss)
          | RClause(id, expr) -> SRClause(id, check_expr expr)
          end in
        SForS(f, check_stmt stmt)
      | LoopS(ctrl) -> 
        let l = 
        begin
        match ctrl with
        | BreakS(b) -> SBreakS(b)
        | ContinueS(c) -> SContinueS(c)
        end in
        SLoopS(l)
      | FallS(_) -> SFallS(0)
      
    
    and check_simple_stmt =function
        ExprS e -> SExprS(check_expr e)
      
        
    in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = check_stmt_list func.body
    }
  in
    let check_unit unt =
      let rec check_num_expr = function
        IntLit (l, u) -> ((Int, u), SIntLit l)
      | BoolLit l -> ((Bool, []), SBoolLit l)
      | FloatLit (l, u) -> ((Float, []), SFloatLit l)
      | CharLit l -> ((Char, []), SCharLit l)
      | StrLit l -> ((Str, []), SStrLit l)
      | Id var -> ((type_of_identifier var global_vars, unit_of_identifier var global_vars), SId var)
      | Binop(e1, bop, e2) as e ->
        let ((t1, u1), e1') = check_num_expr e1
        and ((t2, u2), e2') = check_num_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ string_of_unit_expr u1 ^ " " ^ string_of_bop bop ^ " " ^
                  string_of_typ t2 ^ string_of_unit_expr u2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match bop with
              Add | Sub when t1 = Int -> Int
            | Equal | Neq -> Bool
            | Less when t1 = Int -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (* TODO *)
          let check_unit_bop u1 u2 bop = u1 in
          ((t, check_unit_bop u1 u2 bop), SBinop(((t1, u1), e1'), bop, ((t2, u2), e2')))
        else raise (Failure err)
      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _, fu) e =
               let ((et, eu), e') = check_num_expr e in
               let err = "illegal argument found " ^ string_of_typ et ^ " " ^ string_of_unit_expr eu ^
                         " expected " ^ string_of_typ ft ^ " " ^ string_of_unit_expr fu ^ " in " ^ string_of_expr e
               in ((check_type_assign ft et err, check_unit_assign fu eu err), e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
        | _ as l -> raise (Failure ("Invalid operation for unit declaration: " ^ (string_of_expr l)))
    in
      match snd unt with
      | BaseUnit -> (fst unt, SBaseUnit)
      | AUnit l -> (fst unt, SAUnit l)
      | CUnit (e, id) -> (fst unt, SCUnit(check_num_expr e, id))
  in
  (globals, List.map check_unit units, vtypes, List.map check_func functions)
