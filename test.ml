
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

let empty_main = {
  srtyp=(Int, []);
  sfname="main";
  sformals=[];
  slocals=[];
  sbody=[];
};;

let (test_cases_parser : (string * (string * program)) list) = [
  ("global variable declaration", ("int a;", ([(Int, "a", [])], [], [], [])));
];;

let (test_cases_checker : (string * (string * sprogram)) list) = [
  ("global variable declaration", ("int a; int main(){}", ([(Int, "a", [])], [], [], [empty_main])));
];;

let test_compiler =
  let map_parser (item:(string * (string * program))) =
    let parse (s:string) = Gosciparse.program Scanner.token (Lexing.from_string s)
    in
    ("parser: " ^ (fst item)) >:: (fun _ -> assert_equal (snd (snd item)) (parse (fst (snd item))))
  in
  let test_param_parser = List.map map_parser test_cases_parser in
  let map_checker (item:(string * (string * sprogram))) =
    let scheck (s:string) = Semant.check (Gosciparse.program Scanner.token (Lexing.from_string s))
    in
    ("checker: " ^ (fst item)) >:: (fun _ -> assert_equal (snd (snd item)) (scheck (fst (snd item))))
  in
  let test_param_checker = List.map map_checker test_cases_checker in
  ("test suite" >::: (test_param_parser @ test_param_checker));;

let _ = run_test_tt_main test_compiler;;



(* let test1= string_of_tokenseq (Gosciparse.tokenseq Scanner.token (Lexing.from_string "c=a+b"));;

print_endline test1;;

let test_scanner = "test suite for scanner" >::: [
  "1" >:: (fun _ -> assert_equal "ID(c) ASSIGN ID(a) PLUS ID(b) " test1);
]

let test2= Gosciparse.program Scanner.token (Lexing.from_string "int a;");;

let test_parser = "test suite for parser" >::: [
  "1" >:: (fun _ -> assert_equal ([(Int,"a")], [], [], []) test2);

]

let _ = run_test_tt_main test_scanner *)