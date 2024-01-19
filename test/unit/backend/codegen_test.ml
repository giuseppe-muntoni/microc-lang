open OUnit2
open Microc
open Test_utils

let well_formed_module_test sample _ = 
  let program = Utils.create_ast sample in 
  let program = Semantic_analysis.check_semantic program in 
  let llmodule = Codegen.to_llvm_module sample program in 
  let result = Llvm_analysis.verify_module llmodule in 
  match result with 
  | None -> ()
  | Some error -> assert_bool error false
  
let successful_tests = 
  let files = Array.to_list (Sys.readdir "../../../../../test/samples") in 
  let files = List.filter (String.starts_with ~prefix: "test-") files in 
  let tests = List.map (
    fun file -> 
      (String.concat " " ["well-formed module"; file]) >:: (well_formed_module_test file)
  ) files in 
  "Successful tests for module Codegen" >::: tests

let _ = run_test_tt_main successful_tests;
