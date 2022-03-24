open Ast
open Sast

let _ =
  if Array.length Sys.argv != 2 then raise (Failure("Must pass one arg to specify the compiler action: choose from 'scan', 'parse', or 'scheck'"))
  else
    let lexbuf = Lexing.from_channel stdin in
    if Sys.argv.(1) = "scan" then 
      let tokenseq = Gosciparse.tokenseq Scanner.token lexbuf in
      print_endline (string_of_tokenseq tokenseq)
    else if Sys.argv.(1) = "parse" then
      let program = Gosciparse.program Scanner.token lexbuf in
      print_endline (string_of_program program)
    else if Sys.argv.(1) = "scheck" then
      let program = Gosciparse.program Scanner.token lexbuf in
      let sprogram = Semant.check program in
      print_endline (string_of_sprogram sprogram)
    else
      raise (Failure("Invalid action: choose from 'scan', 'parse', or 'scheck'"))