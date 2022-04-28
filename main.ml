open Ast
open Sast

let _ =
  if Array.length Sys.argv < 2
  then
    raise
      (Failure
         "Must pass one arg to specify the compiler action: choose from 'scan', 'parse', \
          'scheck', 'irgen', 'codegen', 'compile', or 'run'")
  else (
    let lexbuf = Lexing.from_channel stdin in
    if Sys.argv.(1) = "scan"
    then (
      let tokenseq = Gosciparse.tokenseq Scanner.token lexbuf in
      print_endline (string_of_tokenseq tokenseq))
    else if Sys.argv.(1) = "parse"
    then (
      let program = Gosciparse.program Scanner.token lexbuf in
      print_endline (string_of_program program))
    else if Sys.argv.(1) = "scheck"
    then (
      let program = Gosciparse.program Scanner.token lexbuf in
      let sprogram = Semant.check program in
      print_endline (string_of_sprogram sprogram))
    else if Sys.argv.(1) = "irgen"
    then (
      let program = Gosciparse.program Scanner.token lexbuf in
      let sprogram = Semant.check program in
      let ir = Irgen.translate sprogram in
      print_string (Llvm.string_of_llmodule ir))
    else if Sys.argv.(1) = "codegen"
    then
      if Array.length Sys.argv >= 3
      then (
        let program = Gosciparse.program Scanner.token lexbuf in
        let sprogram = Semant.check program in
        let ir = Irgen.translate sprogram in
        Llvm.print_module "tmp.ll" ir;
        let fn = Sys.argv.(2) in
        let _ = Sys.command ("llc tmp.ll -o " ^ fn ^ ".s") in
        ignore (Sys.command "rm tmp.ll"))
      else raise (Failure "Must specify output filename for action compile")
    else if Sys.argv.(1) = "compile"
    then
      if Array.length Sys.argv >= 3
      then (
        let program = Gosciparse.program Scanner.token lexbuf in
        let sprogram = Semant.check program in
        let ir = Irgen.translate sprogram in
        Llvm.print_module "tmp.ll" ir;
        let retcode = Sys.command "llc tmp.ll -o tmp.s -O 0" in
        if retcode = 0
        then (
          let fn = Sys.argv.(2) in
          let _ = Sys.command ("gcc -o " ^ fn ^ " tmp.s") in
          ignore (Sys.command "rm tmp.ll tmp.s"))
        else raise (Failure "Failed to generate assembly code"))
      else raise (Failure "Must specify output filename for action compile")
    else if Sys.argv.(1) = "run"
    then (
      let program = Gosciparse.program Scanner.token lexbuf in
      let sprogram = Semant.check program in
      let ir = Irgen.translate sprogram in
      Llvm.print_module "tmp.ll" ir;
      let _ = Sys.command "lli tmp.ll" in
      ignore (Sys.command "rm tmp.ll"))
    else
      raise
        (Failure
           "Invalid action: choose from 'scan', 'parse', 'scheck', 'irgen', 'codegen', \
            'compile', or 'run'"))
;;
