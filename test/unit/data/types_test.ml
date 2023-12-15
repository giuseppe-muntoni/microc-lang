open OUnit2
open Microc

let primitve_datatype_conversion_test _ = 
  let open Types in
  let void_type = Types.convert_to_data_type Ast.TypV in 
  let bool_type = Types.convert_to_data_type Ast.TypB in 
  let int_type = Types.convert_to_data_type Ast.TypI in
  assert_equal (PrimitiveType VoidType) void_type;
  assert_equal (PrimitiveType BoolType) bool_type;
  assert_equal (PrimitiveType(Number IntType)) int_type

let pointer_conversion_test_1 _ = 
  let open Types in 
  let ptr = Types.convert_to_data_type (Ast.TypP(Ast.TypP(Ast.TypI))) in
  assert_equal (CompoundType(Pointer{primitive_type = Number IntType; ptr_indirection = 2})) ptr

let pointer_conversion_test_2 _ = 
  let open Types in 
  let ptr = Types.convert_to_data_type (Ast.TypP(Ast.TypI)) in
  assert_equal (CompoundType(Pointer{primitive_type = Number IntType; ptr_indirection = 1})) ptr

let array_conversion_test_1 _ = 
  let open Types in
  let arr = Types.convert_to_data_type (Ast.TypA(Ast.TypB, Some 5)) in 
  assert_equal (CompoundType(Array{primitive_type = BoolType; ptr_indirection = 0; dimensions = 1; sizes = [(1, Some 5)]})) arr

let array_conversion_test_2 _ = 
  let open Types in
  let arr = Types.convert_to_data_type (Ast.TypA(Ast.TypB, None)) in 
  assert_equal (CompoundType(Array{primitive_type = BoolType; ptr_indirection = 0; dimensions = 1; sizes = [(1, None)]})) arr

let array_conversion_test_3 _ = 
  let open Types in
  let arr = Types.convert_to_data_type (Ast.TypA(Ast.TypA(Ast.TypC, Some 5), Some 7)) in 
  assert_equal (CompoundType(Array{primitive_type = CharType; ptr_indirection = 0; dimensions = 2; sizes = [(1, Some 7); (2, Some 5)]})) arr

let array_conversion_test_4 _ = 
  let open Types in
  let arr = Types.convert_to_data_type (Ast.TypA(Ast.TypA(Ast.TypP(Ast.TypC), Some 5), Some 7)) in 
  assert_equal (CompoundType(Array{primitive_type = CharType; ptr_indirection = 1; dimensions = 2; sizes = [(1, Some 7); (2, Some 5)]})) arr

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
