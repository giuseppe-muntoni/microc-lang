open OUnit2
open Microc
open Test_utils

let with_return_test sample _ = 
  let program = Utils.create_ast sample in 
  let res = Symbol_tables_repository.create program in 
  match res with 
  | Error(err) ->
    assert_bool (Semantic_errors.show (snd err)) false 
  | Ok() ->
    let res = Return_analyzer.check_return_presence program in 
    match res with 
    | Error(err) -> 
      assert_bool (Semantic_errors.show (snd err)) false 
    | Ok() -> ()

let without_return_test sample fname _ = 
  let program = Utils.create_ast sample in 
  let res = Symbol_tables_repository.create program in 
  match res with 
  | Error(err) ->
    assert_bool (Semantic_errors.show (snd err)) false 
  | Ok() ->
    let res = Return_analyzer.check_return_presence program in 
    match res with 
    | Error(err) -> 
      let open Semantic_errors in 
      assert_equal (ReturnAnalyzerErr(NoReturn fname)) (snd err)
    | Ok() -> 
      assert_bool 
      "The program contains a function in which for some execution a return can be missing but instead is ok" 
      false


let with_return_tests = 
  "Tests for return detection" >::: [
    "test-return1" >:: with_return_test "test-return1"
  ]

let without_return_tests = 
  "Tests for missing return" >::: [
    "fail-no-return1" >:: without_return_test "fail-no-return1" "foo";
    "fail-no-return2" >:: without_return_test "fail-no-return2" "foo";
    "fail-no-return3" >:: without_return_test "fail-no-return3" "foo"
  ] 

let _ = 
  run_test_tt_main with_return_tests;
  run_test_tt_main without_return_tests

