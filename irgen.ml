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

type control_target =
  { break_target : L.llbasicblock option
  ; continue_target : L.llbasicblock option
  ; fall_target : L.llbasicblock option
  }

(* translate : Sast.program -> Llvm.module *)
let translate (globals, units, utypes, functions) =
  (* sast utility functions *)
  let unitless_bind (t, v, u) = t, v in
  let unitless_bind_list (bs : A.bind list) = List.map unitless_bind bs in
  let context = L.global_context () in
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "GoSci" in
  (* Get types from the context *)
  let i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context in
  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | _ -> raise (Failure "Type Not Implemented")
  in
  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n, u) =
      let init = L.const_int (ltype_of_typ t) 0 in
      StringMap.add n (L.define_global n init the_module) m
    in
    List.fold_left global_var StringMap.empty globals
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
    let rec build_expr builder ((_, e) : sexpr) =
      match e with
      | SIntLit i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SId s -> L.build_load (lookup s) s builder
      | SAssign (s, e) ->
        let e' = build_expr builder e in
        ignore (L.build_store e' (lookup s) builder);
        e'
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
        | A.Add -> L.build_add
        | A.Sub -> L.build_sub
        | A.And -> L.build_and
        | A.Or -> L.build_or
        | A.Equal -> L.build_icmp L.Icmp.Eq
        | A.Neq -> L.build_icmp L.Icmp.Ne
        | A.Less -> L.build_icmp L.Icmp.Slt
        | _ -> raise (Failure "Operator Not Implemented"))
          e1'
          e2'
          "tmp"
          builder
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
    let rec build_stmt target builder = function
      | SBlock sl -> List.fold_left (build_stmt target) builder sl
      | SExprS e ->
        ignore (build_expr builder e);
        builder
      | SLabelS _ -> raise (Failure "Label statement is not implemented")
      | SReturnS es ->
        (* Fix: each funciton has exactly one return value *)
        ignore (L.build_ret (build_expr builder (List.hd es)) builder);
        builder
      | SIfS (opt, predicate, then_stmt, else_stmt_opt) ->
        (match opt with
        | None -> ()
        | Some exp -> ignore (build_expr builder exp));
        let bool_val = build_expr builder predicate in
        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt target (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        (match else_stmt_opt with
        | Some else_stmt ->
          ignore (build_stmt target (L.builder_at_end context else_bb) else_stmt)
        | None -> ());
        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in
        (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;
        ignore (L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb
      | SForS (fortype, body) ->
        (* 
          1. for (b) {}
            br Cond
          Cond:
            b.code
            br b.addr Loop END
          Loop:
            ...
            br Cond
          END:
            ...
          
          2. for (init stmt opt; cond expr opt; update expr opt) {}
            (init stmt)
            br Cond
          (
          Cond:
            cond.code
            br cond.addr Loop END
          /
          Cond:
            br Loop
          )
            
          Loop:
            ...
            br update
          Update:
            (update.code)
            br Cond
          End:
            ...

          3. for (var: exp) {}
            Not implemented
         *)
        (match fortype with
        | SCondition predicate ->
          let cond_bb = L.append_block context "for_cond" the_function in
          let build_br_cond = L.build_br cond_bb in
          (* partial function *)
          ignore (build_br_cond builder);
          let cond_builder = L.builder_at_end context cond_bb in
          let bool_val = build_expr cond_builder predicate in
          let loop_bb = L.append_block context "for_loop" the_function in
          let end_bb = L.append_block context "for_end" the_function in
          let for_target =
            { break_target = Some end_bb
            ; continue_target = Some cond_bb
            ; fall_target = target.fall_target
            }
          in
          add_terminal
            (build_stmt for_target (L.builder_at_end context loop_bb) body)
            build_br_cond;
          ignore (L.build_cond_br bool_val loop_bb end_bb cond_builder);
          L.builder_at_end context end_bb
        | SFClause (init_stmt, cond_expr, update_expr) ->
          (match init_stmt with
          | None -> ()
          | Some stmt -> ignore (build_stmt target builder stmt));
          let cond_bb = L.append_block context "for_cond" the_function in
          let build_br_cond = L.build_br cond_bb in
          ignore (build_br_cond builder);
          let cond_builder = L.builder_at_end context cond_bb in
          let loop_bb = L.append_block context "for_loop" the_function in
          let update_bb = L.append_block context "for_update" the_function in
          let end_bb = L.append_block context "for_end" the_function in
          let for_target =
            { break_target = Some end_bb
            ; continue_target = Some update_bb
            ; fall_target = target.fall_target
            }
          in
          (match cond_expr with
          | None -> ignore (L.build_br loop_bb cond_builder)
          | Some expr ->
            let bool_val = build_expr cond_builder expr in
            ignore (L.build_cond_br bool_val loop_bb end_bb cond_builder));
          add_terminal
            (build_stmt for_target (L.builder_at_end context loop_bb) body)
            (L.build_br update_bb);
          let update_builder = L.builder_at_end context update_bb in
          (match update_expr with
          | None -> ()
          | Some expr -> ignore (build_expr update_builder expr));
          ignore (build_br_cond update_builder);
          L.builder_at_end context end_bb
        | _ -> raise (Failure "Range-for clause Not implemented"))
      (* loop control statement is guaranteed to be the last statement within a block *)
      (* | SLoopS loo ->
        (match loo with
        | SBreakS b -> 
          ) *)
      | _ -> raise (Failure "Statement Not Implemented")
    in
    let default_target =
      { break_target = None; continue_target = None; fall_target = None }
    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt default_target builder (SBlock fdecl.sbody) in
    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
  in
  List.iter build_function_body functions;
  the_module
;;
