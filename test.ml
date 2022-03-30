
open OUnit2
open Ast
open Sast


let empty_main = {
  srtyp=(Int, []);
  sfname="main";
  sformals=[];
  slocals=[];
  sbody=[];
};;

let (test_cases_parser : (string * (string * program)) list) = [
  ("global int variable declaration without unit", ("int a;", ([(Int, "a", [])], [], [], [])));
  ("global float variable declaration without unit", ("float b;", ([(Float, "b", [])], [], [], [])));
  ("global char variable declaration without unit", ("char c;", ([(Char, "c", [])], [], [], [])));
  ("global string variable declaration without unit", ("string s;", ([(Str, "s", [])], [], [], [])));
  ("global bool variable declaration without unit", ("bool flag;", ([(Bool, "flag", [])], [], [], [])));
  ("global variable declaration with unit", ("float vel [m 1][s -1];",([(Float, "vel",[("m",1);("s",-1)])],[],[],[])));
  ("empty unit",("unit U {}", ([],[("U",BaseUnit)],[],[])));
  ("Non-empty unit",("unit km { 1000 m}", ([],[("km", CUnit(IntLit(1000,[]),"m"))],[],[])));
  ("Vartype declaration",("vartype Num {int | float}",([],[],[("Num",[Int;Float])],[])));
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
