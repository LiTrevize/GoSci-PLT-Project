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
  and float_t = L.double_type context
  and string_t = L.pointer_type (L.i8_type context) in
  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.Str -> string_t
    | A.Char -> i8_t
    | _ -> raise (Failure "Type Not Implemented")
  in
  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n, u, _) =
      let init = L.const_int (ltype_of_typ t) 0 in
      StringMap.add n (L.define_global n init the_module) m
    in
    List.fold_left global_var StringMap.empty sglobals
  in
  let printf_t : L.lltype = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in
  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
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
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
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
    let lookup n =
      try StringMap.find n local_vars with
      | Not_found -> StringMap.find n global_vars
    in
    (* Construct code for an expression; return its value *)
    let rec build_expr builder (((typ, _), e) : sexpr) : L.llvalue =
      match e with
      | SIntLit i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SStrLit s -> L.build_global_stringptr s "tmp_str" builder
      | SFloatLit f -> L.const_float float_t f
      | SCharLit l -> L.const_int i8_t (Char.code l)
      | SId s -> L.build_load (lookup s) s builder
      | SAssign (s, e) ->
        let e' = build_expr builder e in
        (* e' computes the e.addr *)
        ignore (L.build_store e' (lookup s) builder);
        (* store e' to the address of s *)
        e'
      | SUnaop (op, (((t, _), _) as e)) ->
        let v = build_expr builder e in
        (match op with
        | A.Neg when t = A.Int -> L.build_neg
        | A.Neg when t = A.Float -> L.build_fneg
        | A.Not -> L.build_not
        | _ -> raise (Failure "Unary Operator Not Implemented"))
          v
          "tmp"
          builder
      | SBinop (e1, op, e2) ->
        let (t1, _), _ = e1
        and (t2, _), _ = e2
        and e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        if t1 = A.Int && t2 = A.Int
        then
          (match op with
<<<<<<< HEAD
          | A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mul     -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.Mod     -> L.build_srem
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Great   -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          | _         -> raise (Failure "illegal binary operation")
          ) e1' e2' "tmp" builder 
        else if t1 = A.Float || t2 = A.Float then
          (match op with
          | A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mul     -> L.build_fmul
          | A.Div     -> L.build_fdiv
          | A.Mod     -> L.build_srem    
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Olt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Great   -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | _ -> raise (Failure ("illegal usage of operator " ^ (A.string_of_bop op) ^ " on float"))
          ) e1' e2' "tmp" builder
        else if t1 = A.Bool && t2 = A.Bool then
          (match op with
          | A.And    -> L.build_and
          | A.Or     -> L.build_or 
          | A.Equal  -> L.build_icmp L.Icmp.Eq 
          | A.Neq    -> L.build_icmp L.Icmp.Ne
          | _        ->  raise (Failure "illegal boolean binary operation")
          ) e1' e2' "tmp" builder
        else if t1 = A.Char && t2 = A.Char then
          (match op with
          | A.Equal -> L.build_icmp L.Icmp.Eq
          | A.Neq -> L.build_icmp L.Icmp.Ne
          | _        ->  raise (Failure "illegal char binary operation")
          ) e1' e2' "tmp" builder
=======
          | A.Add -> L.build_add
          | A.Sub -> L.build_sub
          | A.Mul -> L.build_mul
          | A.Div -> L.build_sdiv
          | A.Mod -> L.build_srem
          | A.And -> L.build_and
          | A.Or -> L.build_or
          | A.Equal -> L.build_icmp L.Icmp.Eq
          | A.Neq -> L.build_icmp L.Icmp.Ne
          | A.Less -> L.build_icmp L.Icmp.Slt
          | A.Leq -> L.build_icmp L.Icmp.Sle
          | A.Great -> L.build_icmp L.Icmp.Sgt
          | A.Geq -> L.build_icmp L.Icmp.Sge
          | _ -> raise (Failure "illegal binary operation"))
            e1'
            e2'
            "tmp"
            builder
        else if t1 = A.Float || t2 = A.Float
        then
          (match op with
          | A.Add -> L.build_fadd
          | A.Sub -> L.build_fsub
          | A.Mul -> L.build_fmul
          | A.Div -> L.build_fdiv
          | A.Mod -> L.build_srem
          | A.Equal -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq -> L.build_fcmp L.Fcmp.One
          | A.Less -> L.build_fcmp L.Fcmp.Olt
          | A.Leq -> L.build_fcmp L.Fcmp.Ole
          | A.Great -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq -> L.build_fcmp L.Fcmp.Oge
          | _ ->
            raise
              (Failure ("illegal usage of operator " ^ A.string_of_bop op ^ " on float")))
            e1'
            e2'
            "tmp"
            builder
>>>>>>> e14d6237d9a93e9e46415411c9806e1d7882ce36
        else (
          print_endline (A.string_of_typ t1);
          print_endline (A.string_of_typ t2);
          raise (Failure "Binary Expression Not Implemented"))
      | SCall ("print", [ e ]) ->
        L.build_call
          printf_func
          [| int_format_str; build_expr builder e |]
          "printf"
          builder
      | SCall (f, args) ->
        let fdef, fdecl = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
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
    let rec build_stmt builder = function
      | SBlock sl -> List.fold_left build_stmt builder sl
      | SExprS e ->
        ignore (build_expr builder e);
        builder
      | SReturnS es ->
        ignore (L.build_ret (build_expr builder (List.hd es)) builder);
        builder
      | SIfS (opt, predicate, then_stmt, else_stmt_opt) ->
        let bool_val = build_expr builder predicate in
        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        (match else_stmt_opt with
        | Some else_stmt ->
          ignore (build_stmt (L.builder_at_end context else_bb) else_stmt)
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
          let bool_val = build_expr while_builder predicate in
          let body_bb = L.append_block context "while_body" the_function in
          add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;
          let end_bb = L.append_block context "while_end" the_function in
          ignore (L.build_cond_br bool_val body_bb end_bb while_builder);
          L.builder_at_end context end_bb
        | _ -> raise (Failure "For type Not implemented"))
      | _ -> raise (Failure "Statement Not Implemented")
    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in
    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
  in
  List.iter build_function_body functions;
  the_module
;;
