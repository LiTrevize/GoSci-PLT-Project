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
    let scheck lexbuf =
      let program = Gosciparse.program Scanner.token lexbuf in
      Semant.check program
    in
    let irgen filename =
      let sprogram = scheck lexbuf in
      let ir = Irgen.translate sprogram in
      Llvm.print_module filename ir
    in
    let codegen filename =
      irgen "tmp.ll";
      let retcode = Sys.command ("llc  -O0 -relocation-model=pic tmp.ll -o " ^ filename) in
      let _ = Sys.command "rm tmp.ll" in
      retcode
    in
    let compile filename =
      let retcode = codegen "tmp.s" in
      if retcode = 0
      then (
        let _ = Sys.command ("gcc -o " ^ filename ^ " tmp.s") in
        ignore (Sys.command "rm tmp.s"))
      else raise (Failure "Failed to generate assembly code")
    in
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
      let sprogram = scheck lexbuf in
      print_endline (string_of_sprogram sprogram))
    else if Sys.argv.(1) = "irgen"
    then (
      let sprogram = scheck lexbuf in
      let ir = Irgen.translate sprogram in
      print_string (Llvm.string_of_llmodule ir))
    else if Sys.argv.(1) = "codegen"
    then
      if Array.length Sys.argv >= 3
      then (
        let fn = Sys.argv.(2) in
        ignore (codegen fn))
      else raise (Failure "Must specify output filename for action compile")
    else if Sys.argv.(1) = "compile"
    then
      if Array.length Sys.argv >= 3
      then (
        let fn = Sys.argv.(2) in
        ignore (compile fn))
      else raise (Failure "Must specify output filename for action compile")
    else if Sys.argv.(1) = "run"
    then (
      compile "tmp";
      let _ = Sys.command "./tmp" in
      ignore (Sys.command "rm tmp"))
    else
      raise
        (Failure
           "Invalid action: choose from 'scan', 'parse', 'scheck', 'irgen', 'codegen', \
            'compile', or 'run'"))
;;
