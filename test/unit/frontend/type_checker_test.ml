open OUnit2
open Microc
open Test_utils

let well_typed_test _ = 
  let program = Utils.create_ast "test-ex12.mc" in
  let res = Symbol_table_repository.create program in 
  match res with 
  | Error(err) -> 
    assert_bool (Errors.show (snd err)) false 
  | Ok() -> 
    let res = Type_checker.check_types program in 
    match res with 
    | Error(err) -> 
      assert_bool (Errors.show (snd err)) false 
    | Ok() -> 
      ()

let tests = "Tests for module Type_checker" >::: [
    "well-typed program" >:: well_typed_test
]

let _ = run_test_tt_main tests