open OUnit2
open Microc
open Test_utils

let fail_no_array_size_test source arr_id _ = 
  let program = Utils.create_ast source in 
  let res = Symbol_tables_repository.create program in 
  match res with 
  | Error(err) ->
    let open Semantic_errors in 
    assert_equal (DeclarationsErr(ArrayVarWithoutSize arr_id)) (snd(err))
  | Ok() ->
    assert_bool "The program must fail but it is ok" false 

let fail_access_test source error _ = 
  let program = Utils.create_ast source in 
  let res = Symbol_tables_repository.create program in 
  match res with 
  | Error(err) ->
    assert_equal error (snd(err))
  | Ok() ->
    assert_bool "The program must fail but it is ok" false 

let tests = let open Semantic_errors in 
"Tests for module Declarations_analyzer" >::: [
  "no size array local var declaration"   >:: fail_no_array_size_test "fail-no-size-arr1" "x";
  "no size array global var declaration"  >:: fail_no_array_size_test "fail-no-size-arr2" "x";
  "access error: fail-for1"               >:: fail_access_test        "fail-for1"    (DeclarationsErr(NotDeclaredVar("j")));
  "access error: fail-for2"               >:: fail_access_test        "fail-for2"    (DeclarationsErr(NotDeclaredVar("j")));
  "access error: fail-for4"               >:: fail_access_test        "fail-for4"    (DeclarationsErr(NotDeclaredVar("j")));
  "access error: fail-for5"               >:: fail_access_test        "fail-for5"    (DeclarationsErr(NotDeclaredFun("foo")));
  "access error: fail-while2"             >:: fail_access_test        "fail-while2"  (DeclarationsErr(NotDeclaredFun("foo")));
  "access error: fail-if2"                >:: fail_access_test        "fail-if2"     (DeclarationsErr(NotDeclaredVar("foo")));
  "access error: fail-if3"                >:: fail_access_test        "fail-if3"     (DeclarationsErr(NotDeclaredVar("bar")));
  "access error: fail-scope1"             >:: fail_access_test        "fail-scope1"  (DeclarationsErr(NotDeclaredVar("x")));
]

let _ = run_test_tt_main tests