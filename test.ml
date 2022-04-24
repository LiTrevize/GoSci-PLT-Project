open OUnit2
open Ast
open Sast

let empty_main =
  { srtyp = Int, []; sfname = "main"; sformals = []; slocals = []; sbody = [] }
;;

let (test_cases_parser : (string * (string * program)) list) =
  [ ( "global int variable declaration without unit"
    , ("int a;", ([ Int, "a", [] ], [], [], [])) )
  ; ( "global float variable declaration without unit"
    , ("float b;", ([ Float, "b", [] ], [], [], [])) )
  ; ( "global char variable declaration without unit"
    , ("char c;", ([ Char, "c", [] ], [], [], [])) )
  ; ( "global string variable declaration without unit"
    , ("string s;", ([ Str, "s", [] ], [], [], [])) )
  ; ( "global bool variable declaration without unit"
    , ("bool flag;", ([ Bool, "flag", [] ], [], [], [])) )
  ; ( "global variable declaration with unit"
    , ("float vel [m 1][s -1];", ([ Float, "vel", [ "m", 1; "s", -1 ] ], [], [], [])) )
  ; "empty unit", ("unit U {}", ([], [ "U", BaseUnit ], [], []))
  ; ( "Non-empty unit"
    , ("unit km { 1000 m}", ([], [ "km", CUnit (IntLit (1000, []), "m") ], [], [])) )
  ; ( "Vartype declaration"
    , ("vartype Num {int | float}", ([], [], [ VarType ("Num", [ Int; Float ]) ], [])) )
  ; ( "Struct declaration"
    , ( "struct Person {string name; int age;}"
      , ([], [], [ StructType ("Person", [ Str, "name", []; Int, "age", [] ]) ], []) ) )
  ; ( "function declaration"
    , ( "func testfunc1 (int a, int b) int {return a;}"
      , ( []
        , []
        , []
        , [ { rtyp = Int, []
            ; fname = "testfunc1"
            ; formals = [ Int, "a", []; Int, "b", [] ]
            ; locals = []
            ; body = [ ReturnS [ Id "a" ] ]
            }
          ] ) ) )
  ; ( "function declaration"
    , ( "func testfunc2 (int a, int b) int {int c; c = a + b; return c;}"
      , ( []
        , []
        , []
        , [ { rtyp = Int, []
            ; fname = "testfunc2"
            ; formals = [ Int, "a", []; Int, "b", [] ]
            ; locals = [ Int, "c", [] ]
            ; body =
                [ ExprS (Assign ("c", Binop (Id "a", Add, Id "b"))); ReturnS [ Id "c" ] ]
            }
          ] ) ) )
  ; ( "for"
    , ( "func testfunc3 (int a, int b) int { for (a!=b){ a=a-b; } return a;}"
      , ( []
        , []
        , []
        , [ { rtyp = Int, []
            ; fname = "testfunc3"
            ; formals = [ Int, "a", []; Int, "b", [] ]
            ; locals = []
            ; body =
                [ ForS
                    ( Condition (Binop (Id "a", Neq, Id "b"))
                    , Block [ ExprS (Assign ("a", Binop (Id "a", Sub, Id "b"))) ] )
                ; ReturnS [ Id "a" ]
                ]
            }
          ] ) ) )
  ; ( "if"
    , ( "func testfunc4 (int a, int b) int { if (b< a) { a= a-b;} else { b= b-a;} return \
         a;}"
      , ( []
        , []
        , []
        , [ { rtyp = Int, []
            ; fname = "testfunc4"
            ; formals = [ Int, "a", []; Int, "b", [] ]
            ; locals = []
            ; body =
                [ IfS
                    ( None
                    , Binop (Id "b", Less, Id "a")
                    , Block [ ExprS (Assign ("a", Binop (Id "a", Sub, Id "b"))) ]
                    , Some (Block [ ExprS (Assign ("b", Binop (Id "b", Sub, Id "a"))) ])
                    )
                ; ReturnS [ Id "a" ]
                ]
            }
          ] ) ) )
  ; ( "match"
    , ( "vartype Num {int|float} func testfunc5 () int { Num n; match (v:=n) { case int: \
         return 1; case float: return 2; default: break;}}"
      , ( []
        , []
        , [ VarType ("Num", [ Int; Float ]) ]
        , [ { rtyp = Int, []
            ; fname = "testfunc5"
            ; formals = []
            ; locals = [ UserType "Num", "n", [] ]
            ; body =
                [ MatchS
                    ( None
                    , "v"
                    , Id "n"
                    , [ MatchC (Some Int, [ ReturnS [ IntLit (1, []) ] ])
                      ; MatchC (Some Float, [ ReturnS [ IntLit (2, []) ] ])
                      ; MatchC (None, [ LoopS (BreakS None) ])
                      ] )
                ]
            }
          ] ) ) )
  ; ( "switch"
    , ( "func testfunc6 () int {int x; x=1; switch (x+1;x) {case 1, 2: return x; case 3: \
         break; default: return x-1;}}"
      , ( []
        , []
        , []
        , [ { rtyp = Int, []
            ; fname = "testfunc6"
            ; formals = []
            ; locals = [ Int, "x", [] ]
            ; body =
                [ ExprS (Assign ("x", IntLit (1, [])))
                ; SwitchS
                    ( Some (Binop (Id "x", Add, IntLit (1, [])))
                    , Some (Id "x")
                    , [ CaseS ([ IntLit (1, []); IntLit (2, []) ], [ ReturnS [ Id "x" ] ])
                      ; CaseS ([ IntLit (3, []) ], [ LoopS (BreakS None) ])
                      ; CaseS ([], [ ReturnS [ Binop (Id "x", Sub, IntLit (1, [])) ] ])
                      ] )
                ]
            }
          ] ) ) )
  ]
;;

let (test_cases_checker : (string * (string * sprogram)) list) =
  [ ( "global variable declaration"
    , ("int a; func main() int {}", ([ Int, "a", [] ], [], [], [ empty_main ])) )
  ; ( "global float variable declaration without unit"
    , ("float b; func main() int {}", ([ Float, "b", [] ], [], [], [ empty_main ])) )
  ; ( "global char variable declaration without unit"
    , ("char c; func main() int {}", ([ Char, "c", [] ], [], [], [ empty_main ])) )
  ; ( "global string variable declaration without unit"
    , ("string s; func main() int {}", ([ Str, "s", [] ], [], [], [ empty_main ])) )
  ; ( "global bool variable declaration without unit"
    , ("bool flag; func main() int {}", ([ Bool, "flag", [] ], [], [], [ empty_main ])) )
  ; ( "global variable declaration with unit"
    , ( "float vel [m 1][s -1]; func main() int {}"
      , ([ Float, "vel", [ "m", 1; "s", -1 ] ], [], [], [ empty_main ]) ) )
  ; ( "Non-empty unit"
    , ( "unit km { 1000 m } func main() int {}"
      , ([], [ "km", SCUnit (((Int, []), SIntLit 1000), "m") ], [], [ empty_main ]) ) )
  ; ( "Vartype declaration"
    , ( "vartype Num {int | float} func main() int {}"
      , ([], [], [ SVarType ("Num", [ Int; Float ]) ], [ empty_main ]) ) )
  ; ( "Struct declaration"
    , ( "struct Person {string name; int age;} func main() int {}"
      , ( []
        , []
        , [ SStructType ("Person", [ Str, "name", []; Int, "age", [] ]) ]
        , [ empty_main ] ) ) )
  ; ( "function declaration"
    , ( "func testfunc1 (int a, int b) int {return a;} func main() int {}"
      , ( []
        , []
        , []
        , [ { srtyp = Int, []
            ; sfname = "testfunc1"
            ; sformals = [ Int, "a", []; Int, "b", [] ]
            ; slocals = []
            ; sbody = [ SReturnS [ (Int, []), SId "a" ] ]
            }
          ; empty_main
          ] ) ) )
  ]
;;

let (test_cases_stmt_checker : (string * (string * sstmt list)) list) =
  [ ( "for"
    , ( "func main(int a, int b) int { for (a!=b){ a=a-b; } return a;}"
      , [ SForS
            ( SCondition
                ((Bool, []), SBinop (((Int, []), SId "a"), Neq, ((Int, []), SId "b")))
            , SBlock
                [ SExprS
                    ( (Int, [])
                    , SAssign
                        ( "a"
                        , ( (Int, [])
                          , SBinop (((Int, []), SId "a"), Sub, ((Int, []), SId "b")) ) )
                    )
                ] )
        ; SReturnS [ (Int, []), SId "a" ]
        ] ) )
  ; ( "if"
    , ( "func main(int a, int b) int { if (b< a) { a= a-b;} else { b= b-a;} return a;}"
      , [ SIfS
            ( None
            , ((Bool, []), SBinop (((Int, []), SId "b"), Less, ((Int, []), SId "a")))
            , SBlock
                [ SExprS
                    ( (Int, [])
                    , SAssign
                        ( "a"
                        , ( (Int, [])
                          , SBinop (((Int, []), SId "a"), Sub, ((Int, []), SId "b")) ) )
                    )
                ]
            , Some
                (SBlock
                   [ SExprS
                       ( (Int, [])
                       , SAssign
                           ( "b"
                           , ( (Int, [])
                             , SBinop (((Int, []), SId "b"), Sub, ((Int, []), SId "a")) )
                           ) )
                   ]) )
        ; SReturnS [ (Int, []), SId "a" ]
        ] ) )
  ; ( "match"
    , ( "vartype Num {int|float} func main() int { Num n; match (v:=n) { case int: \
         return 1; case float: return 2; default: break;}}"
      , [ SMatchS
            ( None
            , "v"
            , ((UserType "Num", []), SId "n")
            , [ SMatchC (Some Int, [ SReturnS [ (Int, []), SIntLit 1 ] ])
              ; SMatchC (Some Float, [ SReturnS [ (Int, []), SIntLit 2 ] ])
              ; SMatchC (None, [ SLoopS (SBreakS None) ])
              ] )
        ] ) )
  ]
;;

let test_compiler =
  let parse (s : string) = Gosciparse.program Scanner.token (Lexing.from_string s) in
  let scheck (s : string) = Semant.check (parse s) in
  let map_parser (item : string * (string * program)) =
    "parser: " ^ fst item
    >:: fun _ -> assert_equal (snd (snd item)) (parse (fst (snd item)))
  in
  let test_param_parser = List.map map_parser test_cases_parser in
  let map_checker (item : string * (string * sprogram)) =
    "checker: " ^ fst item
    >:: fun _ -> assert_equal (snd (snd item)) (scheck (fst (snd item)))
  in
  let test_param_checker = List.map map_checker test_cases_checker in
  let map_stmt_checker (item : string * (string * sstmt list)) =
    let _, _, _, funcs = scheck (fst (snd item)) in
    "checker: " ^ fst item
    >:: fun _ ->
    assert_equal (snd (snd item)) (List.find (fun f -> f.sfname = "main") funcs).sbody
  in
  let test_param_stmt_checker = List.map map_stmt_checker test_cases_stmt_checker in
  "test suite" >::: test_param_parser @ test_param_checker @ test_param_stmt_checker
;;

let _ = run_test_tt_main test_compiler
