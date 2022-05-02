(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/
*)

module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)

(* translate : Sast.program -> Llvm.module *)
let translate ((sglobals, units, utypes, functions) : sprogram) =
  (* sast utility functions *)
  let unitless_bind (t, v, u, _) = t, v in
  let unitless_bind_list (bs : sbind list) = List.map unitless_bind bs in
  let context = L.global_context () in
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "GoSci" in
  (* Get types from the context *)
  let i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and i_t n = L.integer_type context n
  and float_t = L.double_type context
  and string_t = L.pointer_type (L.i8_type context) in
  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n, u, _) =
      let init = function
        | A.Int -> L.const_int i32_t 0
        | A.Char -> L.const_int i8_t 0
        | A.Bool -> L.const_int i1_t 0
        | A.Float -> L.const_float float_t 0.0
        | A.Str -> L.const_stringz context ""
        | _ -> raise (Failure (A.string_of_typ t ^ " not supported as global variable"))
      in
      StringMap.add n (L.define_global n (init t) the_module) m
    in
    List.fold_left global_var StringMap.empty sglobals
  in
  (* Return the LLVM type for any GoSci type *)
  (* let ltype_of_native_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.Str -> string_t
    | A.Char -> i8_t
    | _ as t -> raise (Failure ("Type " ^ A.string_of_typ t ^ " is not a native type"))
  in *)
  let size_of_native_typ = function
    | A.Int -> 32
    | A.Bool -> 1
    | A.Float -> 64
    | A.Char -> 8
    | _ as t -> raise (Failure ("Cannot get size of type " ^ A.string_of_typ t))
  in
  (* Create a map of variant type *)
  let vartype_map : L.lltype StringMap.t =
    let add_vartype m = function
      | SVarType (name, typ_list) ->
        (* let ltype_list = List.map ltype_of_native_typ typ_list in *)
        let max_size =
          List.fold_left
            (fun prev typ ->
              let curr = size_of_native_typ typ in
              if prev < curr then curr else prev)
            0
            typ_list
        in
        let struct_t = L.named_struct_type context name in
        L.struct_set_body struct_t [| i32_t; i_t max_size |] false;
        StringMap.add name struct_t m
      | _ -> m
    in
    List.fold_left add_vartype StringMap.empty utypes
  in
  (* Create a map of vartype subtyp list *)
  let vtyp_list_map : A.typ list StringMap.t =
    List.fold_left
      (fun m utyp ->
        match utyp with
        | SVarType (name, typ_list) -> StringMap.add name typ_list m
        | _ -> m)
      StringMap.empty
      utypes
  in
  let type_in_vartype (typ : A.typ) (vt : A.typ) : int =
    match vt with
    | UserType vtn ->
      let typ_list = StringMap.find vtn vtyp_list_map in
      let idx, _ =
        List.fold_left
          (fun (tarid, curid) t -> if typ = t then curid, curid + 1 else tarid, curid + 1)
          (0, 1)
          typ_list
      in
      idx
    | _ -> raise (Failure (A.string_of_typ vt ^ " not a variant type"))
  in
  (* Return the LLVM type for any GoSci type *)
  let ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.Str -> string_t
    | A.Char -> i8_t
    | A.UserType name -> StringMap.find name vartype_map
  in
  let printf_t : L.lltype = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in
  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let poweri_t : L.lltype = L.function_type i32_t [| i32_t; i32_t |] in
  let poweri_func : L.llvalue = L.declare_function "pow" poweri_t the_module in
  let powerf_t : L.lltype = L.function_type float_t [| float_t; float_t |] in
  let powerf_func : L.llvalue = L.declare_function "pow" powerf_t the_module in
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list
          (List.map (fun (t, _) -> ltype_of_typ t) (unitless_bind_list fdecl.sformals))
      in
      let ftype = L.function_type (ltype_of_typ (fst fdecl.srtyp)) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
    List.fold_left function_decl StringMap.empty functions
  in
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let the_function, _ = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let int_format_str = L.build_global_stringptr "%d\n" "fmt_int" builder in
    let float_format_str = L.build_global_stringptr "%g\n" "fmt_float" builder in
    let char_format_str = L.build_global_stringptr "%c\n" "fmt_char" builder in
    let str_format_str = L.build_global_stringptr "%s\n" "fmt_str" builder in
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars : L.llvalue StringMap.t =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m
      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder in
        StringMap.add n local_var m
      in
      let formals =
        List.fold_left2
          add_formal
          StringMap.empty
          (unitless_bind_list fdecl.sformals)
          (Array.to_list (L.params the_function))
      in
      List.fold_left add_local formals (unitless_bind_list fdecl.slocals)
    in
    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup (local_vars : L.llvalue StringMap.t) (n : string) : L.llvalue =
      try StringMap.find n local_vars with
      | Not_found -> StringMap.find n global_vars
    in
    (* Construct code for an expression; return its value *)
    let rec build_expr local_vars builder (((typ, _), e) : sexpr) : L.llvalue =
      match e with
      | SIntLit i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SStrLit s -> L.build_global_stringptr s "tmp_str" builder
      | SFloatLit f -> L.const_float float_t f
      | SCharLit l -> L.const_int i8_t (Char.code l)
      | SId s -> L.build_load (lookup local_vars s) s builder
      (* | SId s -> lookup local_vars s *)
      | SAssign (s, e) ->
        let (rt, _), _ = e in
        let lt, _, _, _ =
          List.find (fun (t, n, u, e) -> n = s) (fdecl.slocals @ fdecl.sformals)
        in
        (* e' computes the e.addr *)
        let e' = build_expr local_vars builder e in
        (match lt with
        | Int | Float | Bool | Char | Str ->
          (* store e' to the address of s *)
          ignore (L.build_store e' (lookup local_vars s) builder);
          e'
        | UserType name ->
          let tid = type_in_vartype rt (UserType name) in
          let var = lookup local_vars s in
          let ptr_tid = L.build_struct_gep var 0 (s ^ ".tid_ptr") builder in
          ignore (L.build_store (L.const_int i32_t tid) ptr_tid builder);
          let data_ptr = L.build_struct_gep var 1 (s ^ ".data_ptr") builder in
          let data_ptr_cast =
            L.build_bitcast
              data_ptr
              (L.pointer_type (ltype_of_typ rt))
              (s ^ ".data_ptr_cast")
              builder
          in
          ignore (L.build_store e' data_ptr_cast builder);
          e')
      | SBinop (e1, op, e2) ->
        let (t1, _), _ = e1
        and (t2, _), _ = e2
        and e1' = build_expr local_vars builder e1
        and e2' = build_expr local_vars builder e2 in
        if t1 = A.Int && t2 = A.Int
        then (
          match op with
          | A.Add -> L.build_add e1' e2' "tmp" builder
          | A.Sub -> L.build_sub e1' e2' "tmp" builder
          | A.Mul -> L.build_mul e1' e2' "tmp" builder
          | A.Div -> L.build_sdiv e1' e2' "tmp" builder
          | A.Mod -> L.build_srem e1' e2' "tmp" builder
          | A.Pow -> L.build_call poweri_func [| e1'; e2' |] "exp" builder
          | A.Equal -> L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
          | A.Neq -> L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
          | A.Less -> L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder
          | A.Leq -> L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
          | A.Great -> L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
          | A.Geq -> L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder
          | _ -> raise (Failure "illegal binary operation"))
        else if t1 = A.Float || t2 = A.Float
        then (
          match op with
          | A.Add -> L.build_fadd e1' e2' "tmp" builder
          | A.Sub -> L.build_fsub e1' e2' "tmp" builder
          | A.Mul -> L.build_fmul e1' e2' "tmp" builder
          | A.Div -> L.build_fdiv e1' e2' "tmp" builder
          | A.Mod -> L.build_srem e1' e2' "tmp" builder
          | A.Pow ->
            let _e1' = L.build_sitofp e1' float_t "cast" builder
            and _e2' = L.build_sitofp e2' float_t "cast" builder in
            L.build_call powerf_func [| _e1'; _e2' |] "exp" builder
          | A.Equal -> L.build_fcmp L.Fcmp.Oeq e1' e2' "tmp" builder
          | A.Neq -> L.build_fcmp L.Fcmp.One e1' e2' "tmp" builder
          | A.Less -> L.build_fcmp L.Fcmp.Olt e1' e2' "tmp" builder
          | A.Leq -> L.build_fcmp L.Fcmp.Ole e1' e2' "tmp" builder
          | A.Great -> L.build_fcmp L.Fcmp.Ogt e1' e2' "tmp" builder
          | A.Geq -> L.build_fcmp L.Fcmp.Oge e1' e2' "tmp" builder
          | _ ->
            raise
              (Failure ("illegal usage of operator " ^ A.string_of_bop op ^ " on float")))
        else if t1 = A.Bool && t2 = A.Bool
        then
          (match op with
          | A.And -> L.build_and
          | A.Or -> L.build_or
          | A.Equal -> L.build_icmp L.Icmp.Eq
          | A.Neq -> L.build_icmp L.Icmp.Ne
          | _ -> raise (Failure "illegal boolean binary operation"))
            e1'
            e2'
            "tmp"
            builder
        else if t1 = A.Char && t2 = A.Char
        then
          (match op with
          | A.Equal -> L.build_icmp L.Icmp.Eq
          | A.Neq -> L.build_icmp L.Icmp.Ne
          | _ -> raise (Failure "illegal char binary operation"))
            e1'
            e2'
            "tmp"
            builder
        else (
          print_endline (A.string_of_typ t1);
          print_endline (A.string_of_typ t2);
          raise (Failure "Binary Expression Not Implemented"))
      | SUnaop (op, (((t, _), _) as e)) ->
        let v = build_expr local_vars builder e in
        (match op with
        | A.Neg when t = A.Int -> L.build_neg
        | A.Neg when t = A.Float -> L.build_fneg
        | A.Not -> L.build_not
        | _ -> raise (Failure "Unary Operator Not Implemented"))
          v
          "tmp"
          builder
      | SIodop (e1, op) ->
        let (t, _), _ = e1
        and e1' = build_expr local_vars builder e1
        and one = build_expr local_vars builder ((Int, []), SIntLit 1) in
        (match op with
        | A.Inc when t = A.Int -> L.build_add e1' one "tmp" builder
        | A.Dec when t = A.Int -> L.build_sub e1' one "tmp" builder
        | _ -> raise (Failure "Illegal Increment/Decrement Operation"))
      | SCall ("print", [ e ]) ->
        L.build_call
          printf_func
          [| (match fst (fst e) with
             | Int | Bool -> int_format_str
             | Float -> float_format_str
             | Char -> char_format_str
             | Str -> str_format_str
             | _ ->
               raise
                 (Failure
                    ("print() cannot take argument of type "
                    ^ A.string_of_typ (fst (fst e)))))
           ; build_expr local_vars builder e
          |]
          "printf"
          builder
      | SCall (f, args) ->
        let fdef, fdecl = StringMap.find f function_decls in
        let llargs =
          List.rev (List.map (build_expr local_vars builder) (List.rev args))
        in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
      | _ -> raise (Failure "Expression Not Implemented")
    in
    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt local_vars builder = function
      | SBlock sl -> List.fold_left (build_stmt local_vars) builder sl
      | SExprS e ->
        ignore (build_expr local_vars builder e);
        builder
      | SReturnS es ->
        ignore (L.build_ret (build_expr local_vars builder (List.hd es)) builder);
        builder
      | SIfS (opt, predicate, then_stmt, else_stmt_opt) ->
        let bool_val = build_expr local_vars builder predicate in
        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt local_vars (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        (match else_stmt_opt with
        | Some else_stmt ->
          ignore (build_stmt local_vars (L.builder_at_end context else_bb) else_stmt)
        | None -> ());
        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in
        (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;
        ignore (L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb
      | SForS (fortype, body) ->
        (match fortype with
        | SCondition predicate ->
          let while_bb = L.append_block context "while" the_function in
          let build_br_while = L.build_br while_bb in
          (* partial function *)
          ignore (build_br_while builder);
          let while_builder = L.builder_at_end context while_bb in
          let bool_val = build_expr local_vars while_builder predicate in
          let body_bb = L.append_block context "while_body" the_function in
          add_terminal
            (build_stmt local_vars (L.builder_at_end context body_bb) body)
            build_br_while;
          let end_bb = L.append_block context "while_end" the_function in
          ignore (L.build_cond_br bool_val body_bb end_bb while_builder);
          L.builder_at_end context end_bb
        | _ -> raise (Failure "For type Not implemented"))
      | SMatchS (_, s, e, case_list) ->
        let (vt, _), e' = e in
        let vtn, vval =
          match e' with
          | SId name -> name, lookup local_vars name
          | _ -> raise (Failure "Only vartype variable can be matched")
        in
        (* let e' = build_expr local_vars builder e in *)
        let end_bb = L.append_block context "match_end" the_function in
        (* partial function *)
        let build_br_end = L.build_br end_bb in
        let build_case builder case =
          match case with
          | SMatchC (Some case_t, sl) ->
            (* build case condition *)
            let ptr_tid = L.build_struct_gep vval 0 (vtn ^ ".tid_ptr") builder in
            let val_tid = L.build_load ptr_tid (vtn ^ ".tid") builder in
            let tid = type_in_vartype case_t vt in
            let bool_val =
              L.build_icmp L.Icmp.Eq val_tid (L.const_int i32_t tid) "match_cmp" builder
            in
            (* add varaible *)
            let local = L.build_alloca (ltype_of_typ case_t) s builder in
            let data_ptr = L.build_struct_gep vval 1 (vtn ^ ".data_ptr") builder in
            let data_ptr_cast =
              L.build_bitcast
                data_ptr
                (L.pointer_type (ltype_of_typ case_t))
                (vtn ^ ".data_ptr_cast")
                builder
            in
            let data_val = L.build_load data_ptr_cast (vtn ^ ".data_val") builder in
            ignore (L.build_store data_val local builder);
            let local_vars = StringMap.add s local local_vars in
            (* build case body block *)
            let case_bb = L.append_block context "match_case_body" the_function in
            (* build stmt list *)
            ignore
              (List.fold_left
                 (build_stmt local_vars)
                 (L.builder_at_end context case_bb)
                 sl);
            add_terminal (L.builder_at_end context case_bb) build_br_end;
            let next_case_bb = L.append_block context "match_case_cond" the_function in
            ignore (L.build_cond_br bool_val case_bb next_case_bb builder);
            L.builder_at_end context next_case_bb
          | SMatchC (None, sl) ->
            (* build stmt list
               reuse next_case_bb from last case
            *)
            ignore (List.fold_left (build_stmt local_vars) builder sl);
            add_terminal builder build_br_end;
            L.builder_at_end context end_bb
        in
        List.fold_left build_case builder case_list
      | _ -> raise (Failure "Statement Not Implemented")
    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt local_vars builder (SBlock fdecl.sbody) in
    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
  in
  List.iter build_function_body functions;
  the_module
;;
