open OUnit2
open Microc
open Test_utils

let with_deadcode_test sample stmt_num var_num _ = 
  let program = Utils.create_ast sample in
  let res = Symbol_tables_repository.create program in 
  match res with 
  | Error(err) -> 
    assert_bool (Semantic_errors.show (snd err)) false 
  | Ok() ->  
    let deadcode = Deadcode_analyzer.detect_deadcode program in 
    assert_equal (List.length deadcode) (stmt_num + var_num)

let without_deadcode_test sample _ = 
  let program = Utils.create_ast sample in
  let res = Symbol_tables_repository.create program in 
  match res with 
  | Error(err) -> 
    assert_bool (Semantic_errors.show (snd err)) false 
  | Ok() -> 
    let deadcode = Deadcode_analyzer.detect_deadcode program in
    assert_equal (List.length deadcode) 0

let with_deadcode_tests = 
  "Tests for deadcode detection" >::: [
    "fail-dead1"    >:: with_deadcode_test "fail-dead1"   1 0;
    "fail-dead2"    >:: with_deadcode_test "fail-dead2"   1 0;
    "fail-dead3"    >:: with_deadcode_test "fail-dead3"   5 0;
    "fail-dead4"    >:: with_deadcode_test "fail-dead4"   1 0;
    "fail-unused1"  >:: with_deadcode_test "fail-unused1" 0 1;
    "fail-unused2"  >:: with_deadcode_test "fail-unused2" 0 1;
    "fail-unused3"  >:: with_deadcode_test "fail-unused3" 0 2;
    "fail-unused4"  >:: with_deadcode_test "fail-unused4" 0 1;
    "fail-unused5"  >:: with_deadcode_test "fail-unused5" 0 1;
  ]

let without_deadcode_tests = 
  "Tests for ok programs" >::: [
    "test-ex1"      >:: without_deadcode_test "test-ex1";
    "test-unused1"  >:: without_deadcode_test "test-unused1";
    "test-unused2"  >:: without_deadcode_test "test-unused2";
    "test-unused3"  >:: without_deadcode_test "test-unused3";
  ]

let _ = 
  run_test_tt_main with_deadcode_tests;
  run_test_tt_main without_deadcode_tests;