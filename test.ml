
open OUnit2
open Ast
open Sast


(* let rec sum = function
| [] -> 0
| x :: xs -> x + sum xs *)

(* let tests = "test suite for sum" >::: [
  "empty" >:: (fun _ -> assert_equal 0 (sum []));
  "singleton" >:: (fun _ -> assert_equal 1 (sum [1]));
  "two_elements" >:: (fun _ -> assert_equal 3 (sum [1; 2]));
] *)

let test1= string_of_tokenseq (Gosciparse.tokenseq Scanner.token (Lexing.from_string "c=a+b"));;

print_endline test1;;

let test_scanner = "test suite for scanner" >::: [
  "1" >:: (fun _ -> assert_equal "ID(c) ASSIGN ID(a) PLUS ID(b) " test1);
]

let test2= Gosciparse.program Scanner.token (Lexing.from_string "int a;");;

let test_parser = "test suite for parser" >::: [
  "1" >:: (fun _ -> assert_equal ([(Int,"a")], [], [], []) test2);
]

let _ = run_test_tt_main test_scanner