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
    [CompoundType(Pointer{primitive_type = Number(FloatType); ptr_indirection = 3});
     CompoundType(Array{primitive_type = BoolType; ptr_indirection = 2; dimensions = 1; sizes = [1, None]})
    ]
  ) in
  let updated_scope = Symbol_builder.build_fun foo_ast global_scope in
  match updated_scope with 
  | Error(err) -> 
    failwith "Expected a success but an error was raised" (* TODO: Add call to to_string of error *)
  | Ok(local_scope) ->
    assert_equal foo_symbol (Symbol_table.lookup "foo" local_scope)

let build_var_test _ = 
  let open Types in 
  let global_scope = Symbol_table.empty_table in
  let updated_scope = Symbol_builder.build_var "x" (Ast.TypA(Ast.TypC, Some 32)) global_scope in
  let expected = Symbol.Var(
      CompoundType(Array {
        primitive_type = CharType;
        ptr_indirection = 0;
        dimensions = 1;
        sizes = [(1, Some 32)]
      }), false) in 
  match updated_scope with
  | Error(err) ->
    failwith "Expected a success but an error was raised" (* TODO: Add call to to_string of error *)
  | Ok(updated_scope) -> 
      assert_equal expected (Symbol_table.lookup "x" updated_scope)

let build_extern_var _ = 
  let open Types in 
  let global_scope = Symbol_table.empty_table in
  let updated_scope = Symbol_builder.build_extern_var "x" (Ast.TypA(Ast.TypC, Some 32)) global_scope in
  let expected = Symbol.Var(
      CompoundType(Array {
        primitive_type = CharType;
        ptr_indirection = 0;
        dimensions = 1;
        sizes = [(1, Some 32)]
      }), true) in 
  match updated_scope with
  | Error(err) ->
    failwith "Expected a success but an error was raised" (* TODO: Add call to to_string of error *)
  | Ok(updated_scope) -> 
      assert_equal expected (Symbol_table.lookup "x" updated_scope)
  
let build_vars_test _ = 
  let open Types in 
  let global_scope = Symbol_table.empty_table in
  let updated_scope = Symbol_builder.build_vars [
    (Ast.TypP(Ast.TypP(Ast.TypP(Ast.TypF))), "x");
    (Ast.TypA(Ast.TypP(Ast.TypP(Ast.TypB)), None), "y")
  ] global_scope in 
  let expected_x = (Symbol.Var((CompoundType(Pointer{primitive_type = Number(FloatType); ptr_indirection = 3})), false)) in 
  let expected_y = (Symbol.Var(CompoundType(Array{primitive_type = BoolType; ptr_indirection = 2; dimensions = 1; sizes = [1, None]}), false)) in
  match updated_scope with
  | Error(err) ->
    failwith "Expected a success but an error was raised" (* TODO: Add call to to_string of error *)
  | Ok(updated_scope) -> 
    assert_equal expected_x (Symbol_table.lookup "x" updated_scope);
    assert_equal expected_y (Symbol_table.lookup "y" updated_scope)

let duplicate_entry_error_test _ = 
  let open Types in 
  let global_scope = Symbol_table.empty_table in
  let updated_scope = Symbol_builder.build_vars [
    (Ast.TypP(Ast.TypP(Ast.TypP(Ast.TypF))), "x");
    (Ast.TypA(Ast.TypP(Ast.TypP(Ast.TypB)), None), "x")
  ] global_scope in 
  match updated_scope with
  | Error(Errors.SymbolErr(DuplicateEntry(
      "x", (Symbol.Var(CompoundType(Array{primitive_type = BoolType; ptr_indirection = 2; dimensions = 1; sizes = [1, None]}), false)))
    )) -> ()
  | _ ->
    failwith "Expected duplicate entry error"

let tests = "Tests for module Symbol_builder" >::: [
  "Build fun" >:: build_fun_test;
  "Build var" >:: build_var_test;
  "Build extern var" >:: build_extern_var;
  "Build vars" >:: build_vars_test;
  "Duplicate entry" >:: duplicate_entry_error_test
]

let _ = run_test_tt_main tests
