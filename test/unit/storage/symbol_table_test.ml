open OUnit2
open Microc

let empty_global_scope_test _ = 
  let global_scope = Symbol_table.empty_table in 
  assert_raises (Symbol_table.NotFoundEntry("x")) (fun _ -> Symbol_table.lookup "x" global_scope)

let local_entry_test _ = 
  let scope = Symbol_table.begin_block Symbol_table.empty_table in 
  let scope = Symbol_table.add_entry "x" (Symbol.LocalVar(Types.PrimitiveType Types.VoidType, Location.dummy_code_pos)) scope in 
  let x = Symbol_table.lookup "x" scope in 
  assert_equal (Symbol.LocalVar(Types.PrimitiveType Types.VoidType, Location.dummy_code_pos)) x

let global_entry_test _ = 
  let scope = Symbol_table.empty_table in 
  let scope = Symbol_table.add_entry "x" (Symbol.GlobalVar(Types.PrimitiveType Types.VoidType, false)) scope in 
  let x = Symbol_table.lookup "x" scope in 
  assert_equal (Symbol.GlobalVar(Types.PrimitiveType Types.VoidType, false)) x

let shadow_lookup_test _ = 
  let global_var = Symbol.GlobalVar(Types.PrimitiveType Types.VoidType, true) in 
  let global_scope = Symbol_table.empty_table in 
  let global_scope = Symbol_table.add_entry "x" global_var global_scope in 
  let local_var = Symbol.LocalVar(Types.PrimitiveType Types.BoolType, Location.dummy_code_pos) in 
  let local_scope = Symbol_table.begin_block global_scope in 
  let local_scope = Symbol_table.add_entry "x" local_var local_scope in 
  assert_equal local_var (Symbol_table.lookup "x" local_scope)

let upper_scope_lookup_test _ = 
  let global_fun = Symbol.Fun(Types.VoidType, []) in 
  let global_scope = Symbol_table.empty_table in 
  let global_scope = Symbol_table.add_entry "foo" global_fun global_scope in  
  let local_var = Symbol.LocalVar(Types.PrimitiveType Types.BoolType, Location.dummy_code_pos) in 
  let local_scope_1 = Symbol_table.begin_block global_scope in 
  let local_scope_2 = Symbol_table.begin_block global_scope in
  let local_scope_1 = Symbol_table.add_entry "foo" local_var local_scope_1 in 
  assert_equal local_var (Symbol_table.lookup "foo" local_scope_1);
  assert_equal global_fun (Symbol_table.lookup "foo" local_scope_2)

let entry_not_found_test _ = 
  let global_scope = Symbol_table.empty_table in
  let local_scope = Symbol_table.begin_block global_scope in 
  let local_scope = Symbol_table.add_entry "y" (Symbol.LocalVar(Types.PrimitiveType Types.CharType, Location.dummy_code_pos)) local_scope in 
  assert_raises (Symbol_table.NotFoundEntry("x")) (fun _ -> Symbol_table.lookup "x" local_scope)

let duplicate_entry_test _ = 
  let global_scope = Symbol_table.empty_table in
  let local_scope = Symbol_table.begin_block global_scope in 
  let local_scope = Symbol_table.add_entry "y" (Symbol.LocalVar(Types.PrimitiveType Types.CharType, Location.dummy_code_pos)) local_scope in 
  assert_raises (Symbol_table.DuplicateEntry("y")) (fun _ -> Symbol_table.add_entry "y" (Symbol.LocalVar(Types.PrimitiveType Types.BoolType, Location.dummy_code_pos)) local_scope) 

let tests = "Tests for module Symbol_table" >::: [
  "Empty global scope" >:: empty_global_scope_test;
  "Local entry lookup" >:: local_entry_test;
  "Global entry lookup" >:: global_entry_test;
  "Shadowing test" >:: shadow_lookup_test;
  "Upper scope lookup" >:: upper_scope_lookup_test;
  "Entry not found" >:: entry_not_found_test;
  "Duplicate entry" >:: duplicate_entry_test
]

let _ = run_test_tt_main tests