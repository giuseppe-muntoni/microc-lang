open OUnit2
open Microc

let primitve_datatype_conversion_test _ = 
  let open Types in
  let void_type = Frontend_types_adapter.adapt_ast_type Ast.TypV in 
  let bool_type = Frontend_types_adapter.adapt_ast_type Ast.TypB in 
  let int_type = Frontend_types_adapter.adapt_ast_type Ast.TypI in
  assert_equal (PrimitiveType VoidType) void_type;
  assert_equal (PrimitiveType BoolType) bool_type;
  assert_equal (PrimitiveType(Number IntType)) int_type

let pointer_conversion_test_1 _ = 
  let open Types in 
  let ptr = Frontend_types_adapter.adapt_ast_type (Ast.TypP(Ast.TypP(Ast.TypI))) in
  assert_equal (CompoundType(Pointer{pointed_type = Number IntType; indirection = 2})) ptr

let pointer_conversion_test_2 _ = 
  let open Types in 
  let ptr = Frontend_types_adapter.adapt_ast_type (Ast.TypP(Ast.TypI)) in
  assert_equal (CompoundType(Pointer{pointed_type = Number IntType; indirection = 1})) ptr

let array_conversion_test_1 _ = 
  let open Types in
  let arr = Frontend_types_adapter.adapt_ast_type (Ast.TypA(Ast.TypB, Some 5)) in 
  assert_equal (CompoundType(Array{elements_type = BoolType; indirection = 0; dimensions = 1; sizes = [(1, Some 5)]})) arr

let array_conversion_test_2 _ = 
  let open Types in
  let arr = Frontend_types_adapter.adapt_ast_type (Ast.TypA(Ast.TypB, None)) in 
  assert_equal (CompoundType(Array{elements_type = BoolType; indirection = 0; dimensions = 1; sizes = [(1, None)]})) arr

let array_conversion_test_3 _ = 
  let open Types in
  let arr = Frontend_types_adapter.adapt_ast_type (Ast.TypA(Ast.TypA(Ast.TypC, Some 5), Some 7)) in 
  assert_equal (CompoundType(Array{elements_type = CharType; indirection = 0; dimensions = 2; sizes = [(1, Some 7); (2, Some 5)]})) arr

let array_conversion_test_4 _ = 
  let open Types in
  let arr = Frontend_types_adapter.adapt_ast_type (Ast.TypA(Ast.TypA(Ast.TypP(Ast.TypC), Some 5), Some 7)) in 
  assert_equal (CompoundType(Array{elements_type = CharType; indirection = 1; dimensions = 2; sizes = [(1, Some 7); (2, Some 5)]})) arr

let tests = "Tests for module Types" >::: [
  "Primitive datatype conversion" >:: primitve_datatype_conversion_test;
  "Multi-pointer conversion test" >:: pointer_conversion_test_1;
  "Single-pointer conversion test" >:: pointer_conversion_test_2;
  "Array conversion test" >:: array_conversion_test_1;
  "Array conversion test 2" >:: array_conversion_test_2;
  "Matrix conversion test" >:: array_conversion_test_3;
  "Array of pointers conversion test" >:: array_conversion_test_4
]

let _ = run_test_tt_main tests
