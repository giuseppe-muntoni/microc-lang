open OUnit2
open Microc

let build_fun_test _ = 
  let open Types in
  let open Ast in 
  let global_scope = Symbol_table.empty_table in
  let foo_ast = {
    typ = Ast.TypF;
    fname = "foo";
    formals = [
      (Ast.TypP(Ast.TypP(Ast.TypP(Ast.TypF))), "x");
      ((Ast.TypA(Ast.TypP(Ast.TypP(Ast.TypB)), None)), "y")
   ];
   body = None
  } in
  let foo_symbol = Symbol.Fun(
    Number(FloatType), 
    [(CompoundType(Pointer{pointed_type = Number(FloatType); indirection = 3}), "x");
     (CompoundType(Array{elements_type = BoolType; indirection = 2; dimensions = 1; sizes = [1, None]}), "y")
    ]
  ) in
  let updated_scope = Symbol_builder.build_fun foo_ast global_scope in
  match updated_scope with 
  | Error(err) -> 
    assert_bool (String.concat "" ["Expected a success but this error was returned: "; Semantic_errors.show err]) false 
  | Ok(local_scope) ->
    assert_equal foo_symbol (Symbol_table.lookup "foo" local_scope)

let build_var_test _ = 
  let open Types in 
  let global_scope = Symbol_table.empty_table in
  let updated_scope = Symbol_builder.build_global_var "x" (Ast.TypA(Ast.TypC, Some 32)) global_scope in
  let expected = Symbol.GlobalVar(
      CompoundType(Array {
        elements_type = CharType;
        indirection = 0;
        dimensions = 1;
        sizes = [(1, Some 32)]
      }), false) in 
  match updated_scope with
  | Error(err) ->
    assert_bool (String.concat "" ["Expected a success but this error was returned: "; Semantic_errors.show err]) false 
  | Ok(updated_scope) -> 
      assert_equal expected (Symbol_table.lookup "x" updated_scope)

let build_extern_var _ = 
  let open Types in 
  let global_scope = Symbol_table.empty_table in
  let updated_scope = Symbol_builder.build_global_extern_var "x" (Ast.TypA(Ast.TypC, Some 32)) global_scope in
  let expected = Symbol.GlobalVar(
      CompoundType(Array {
        elements_type = CharType;
        indirection = 0;
        dimensions = 1;
        sizes = [(1, Some 32)]
      }), true) in 
  match updated_scope with
  | Error(err) ->
    assert_bool (String.concat "" ["Expected a success but this error was returned: "; Semantic_errors.show err]) false 
  | Ok(updated_scope) -> 
      assert_equal expected (Symbol_table.lookup "x" updated_scope)
  
let build_vars_test _ = 
  let open Types in 
  let global_scope = Symbol_table.empty_table in
  let updated_scope = Symbol_builder.build_local_vars [
    (Ast.TypP(Ast.TypP(Ast.TypP(Ast.TypF))), "x");
    (Ast.TypA(Ast.TypP(Ast.TypP(Ast.TypB)), None), "y")
  ] Location.dummy_code_pos global_scope in 
  let expected_x = (Symbol.LocalVar((CompoundType(Pointer{pointed_type = Number(FloatType); indirection = 3})), Location.dummy_code_pos)) in 
  let expected_y = (Symbol.LocalVar(CompoundType(Array{elements_type = BoolType; indirection = 2; dimensions = 1; sizes = [1, None]}), Location.dummy_code_pos)) in
  match updated_scope with
  | Error(err) ->
    assert_bool (String.concat "" ["Expected a success but this error was returned: "; Semantic_errors.show err]) false 
  | Ok(updated_scope) -> 
    assert_equal expected_x (Symbol_table.lookup "x" updated_scope);
    assert_equal expected_y (Symbol_table.lookup "y" updated_scope)

let duplicate_entry_error_test _ = 
  let open Types in 
  let global_scope = Symbol_table.empty_table in
  let updated_scope = Symbol_builder.build_local_vars [
    (Ast.TypP(Ast.TypP(Ast.TypP(Ast.TypF))), "x");
    (Ast.TypA(Ast.TypP(Ast.TypP(Ast.TypB)), None), "x")
  ] Location.dummy_code_pos global_scope in 
  match updated_scope with
  | Error(err) -> 
    let expected = Semantic_errors.SymbolErr(Semantic_errors.DuplicateEntry(
      "x", (Symbol.LocalVar(CompoundType(Array{elements_type = BoolType; indirection = 2; dimensions = 1; sizes = [1, None]}), Location.dummy_code_pos)))
    ) in 
    assert_equal ~printer: Semantic_errors.show expected err
  | Ok(_) ->
    assert_bool "Expected a duplicate entry error but ok was returned" false 

let multidim_array_error_test _ = 
  let open Ast in
  let global_scope = Symbol_table.empty_table in
  let updated_scope = Symbol_builder.build_global_var "x" (TypA(TypA(TypI, None), None)) global_scope in 
  match updated_scope with 
  | Error(err) ->
    let expected = Semantic_errors.SymbolErr(Semantic_errors.MultiDimArray "x") in
    assert_equal ~printer: Semantic_errors.show expected err
  | Ok(_) -> 
    assert_bool "Expected a duplicate entry error but ok was returned" false 

let void_var_error_test _ = 
  let global_scope = Symbol_table.empty_table in
  let updated_scope = Symbol_builder.build_global_var "x" Ast.TypV global_scope in 
  match updated_scope with 
  | Error(err) ->
    let expected = Semantic_errors.SymbolErr(Semantic_errors.VoidVarDecl "x") in
    assert_equal ~printer: Semantic_errors.show expected err
  | Ok(_) -> 
    assert_bool "Expected a duplicate entry error but ok was returned" false 

let tests = "Tests for module Symbol_builder" >::: [
  "build fun" >:: build_fun_test;
  "build var" >:: build_var_test;
  "build extern var" >:: build_extern_var;
  "build vars" >:: build_vars_test;
  "duplicate entry" >:: duplicate_entry_error_test;
  "multidim array" >:: multidim_array_error_test;
  "void var" >:: void_var_error_test
]

let _ = run_test_tt_main tests
