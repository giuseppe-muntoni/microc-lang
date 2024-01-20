open OUnit2
open Microc
open Test_utils

let well_typed_test sample _ = 
  let program = Utils.create_ast sample in
  let res = Symbol_tables_repository.create program in 
  match res with 
  | Error(err) -> 
    assert_bool (Semantic_errors.show (snd err)) false 
  | Ok() -> 
    let res = Type_checker.check_types program in 
    match res with 
    | Error(err) -> 
      assert_bool (Semantic_errors.show (snd err)) false 
    | Ok() -> 
      ()

let ill_typed_test sample expected_err _ = 
  let program = Utils.create_ast sample in
  let res = Symbol_tables_repository.create program in 
  match res with 
  | Error(err) -> 
    assert_bool (Semantic_errors.show (snd err)) false 
  | Ok() -> 
    let res = Type_checker.check_types program in 
    match res with 
    | Error(err) -> 
      assert_equal ~printer: (Semantic_errors.show) expected_err (snd err) 
    | Ok() -> 
      assert_bool (String.concat " " ["The type checker must return"; Semantic_errors.show expected_err; "but instead returns Ok"]) false

let ill_symbol_test sample expected_err _ = 
  let program = Utils.create_ast sample in
  let res = Symbol_tables_repository.create program in 
  match res with 
  | Error(err) -> 
    assert_equal ~printer: (Semantic_errors.show) expected_err (snd err) 
  | Ok() -> 
    assert_bool (String.concat " " ["The symbol builder must return"; Semantic_errors.show expected_err; "but instead returns Ok"]) false

let successful_tests = 
  let files = Array.to_list (Sys.readdir "../../../../../test/samples") in 
  let files = List.filter (String.starts_with ~prefix: "test-") files in 
  let tests = List.map (
    fun file -> 
      (String.concat " " ["well-typed program"; file]) >:: (well_typed_test file)
  ) files in 
  "Successful tests for module Type_checker" >::: tests

let failing_tests = 
  let open Semantic_errors in 
  let open Types in 
  "Failing tests for module Type_checker" >::: [
    "ill-typed program: fail-array1"    >:: (ill_symbol_test  "fail-array1"   (SymbolErr(MultiDimArray "a")));
    "ill-typed program: fail-array2"    >:: (ill_symbol_test  "fail-array2"   (SymbolErr(MultiDimArray "b")));
    "ill-typed program: fail-array3"    >:: (ill_symbol_test  "fail-array3"   (SymbolErr(MultiDimArray "a")));
    "ill-typed program: fail-assign1"   >:: (ill_typed_test   "fail-assign1"  (TypeCheckerErr(AssignTypeMismatch(PrimitiveType(Number IntType), PrimitiveType(BoolType)))));
    "ill-typed program: fail-assign2"   >:: (ill_typed_test   "fail-assign2"  (TypeCheckerErr(AssignTypeMismatch(PrimitiveType(BoolType), PrimitiveType(Number IntType)))));
    "ill-typed program: fail-assign3"   >:: (ill_typed_test   "fail-assign3"  (TypeCheckerErr(AssignTypeMismatch(PrimitiveType(Number IntType), PrimitiveType(VoidType)))));
    "ill-typed program: fail-assign4"   >:: (ill_typed_test   "fail-assign4"  (TypeCheckerErr(AssignTypeMismatch(
      CompoundType(Pointer { 
        pointed_type = Number IntType;
        indirection = 1 
      }),
      CompoundType(Array {
        elements_type = Number IntType;
        indirection = 0;
        dimensions = 1; 
        sizes = [(1, (Some 10))] 
      })))));
    "ill-typed program: fail-assign5"   >:: (ill_typed_test   "fail-assign5"  (TypeCheckerErr(AssignTypeMismatch(
      CompoundType(Array {
        elements_type = Number IntType;
        indirection = 0;
        dimensions = 1;
        sizes = [(1, Some 5)]
      }),
      PrimitiveType(Number IntType)))
    ));
    "ill-typed program: fail-assign6"   >:: (ill_typed_test   "fail-assign6"  (TypeCheckerErr(ArrNotAssignable(
      CompoundType(Array {
        elements_type = Number IntType;
        indirection = 0;
        dimensions = 1;
        sizes = [(1, Some 5)]
      })))));
    "ill-typed program: fail-assign7"   >:: (ill_typed_test   "fail-assign7"  (TypeCheckerErr(AssignTypeMismatch(
      CompoundType(Pointer { 
        pointed_type = Number IntType;
        indirection = 1 
      }),
      CompoundType(Array {
        elements_type = Number IntType;
        indirection = 0;
        dimensions = 1; 
        sizes = [(1, (Some 5))] 
      })))));
    "ill-typed program: fail-assign8"   >:: (ill_typed_test   "fail-assign8"  (TypeCheckerErr(WrongBinOpType(Ast.Add))));
    "ill-typed program: fail-expr1"     >:: (ill_typed_test   "fail-expr1"    (TypeCheckerErr(WrongBinOpType(Ast.Add))));
    "ill-typed program: fail-expr2"     >:: (ill_typed_test   "fail-expr2"    (TypeCheckerErr(WrongBinOpType(Ast.Add))));
    "ill-typed program: fail-for3"      >:: (ill_typed_test   "fail-for3"     (TypeCheckerErr(GuardNotBool)));
    "ill-typed program: fail-func1"     >:: (ill_symbol_test  "fail-func1"    (SymbolErr(DuplicateEntry("bar", Symbol.Fun(VoidType, [])))));
    "ill-typed program: fail-func2"     >:: (ill_symbol_test  "fail-func2"    (SymbolErr(DuplicateEntry("a", Symbol.Var(PrimitiveType(Number IntType), false)))));
    "ill-typed program: fail-func3"     >:: (ill_symbol_test  "fail-func3"    (SymbolErr(VoidVarDecl("b"))));
    "ill-typed program: fail-func4"     >:: (ill_symbol_test  "fail-func4"    (SymbolErr(DuplicateEntry("print", Symbol.Fun((Number IntType), [])))));
    "ill-typed program: fail-func5"     >:: (ill_symbol_test  "fail-func5"    (SymbolErr(VoidVarDecl("b"))));
    "ill-typed program: fail-func6"     >:: (ill_typed_test   "fail-func6"    (TypeCheckerErr(WrongActualParamsType("foo",
    [
      PrimitiveType(Number IntType);
      PrimitiveType(BoolType)
    ],
    [
      PrimitiveType(Number(IntType))
    ]))));
    "ill-typed program: fail-func7"     >:: (ill_typed_test   "fail-func7"    (TypeCheckerErr(WrongActualParamsType("foo",
    [
      PrimitiveType(Number IntType);
      PrimitiveType(BoolType)
    ],
    [
      PrimitiveType(Number IntType);
      PrimitiveType(BoolType);
      PrimitiveType(BoolType)
    ]))));
    "ill-typed program: fail-func8"     >:: (ill_typed_test   "fail-func8"    (TypeCheckerErr(WrongActualParamsType("foo",
    [
      PrimitiveType(Number IntType);
      PrimitiveType(BoolType)
    ],
    [
      PrimitiveType(Number IntType);
      PrimitiveType(VoidType);
    ]))));
    "ill-typed program: fail-func9"     >:: (ill_typed_test   "fail-func9"    (TypeCheckerErr(WrongActualParamsType("foo",
    [
      PrimitiveType(Number IntType);
      PrimitiveType(BoolType)
    ],
    [
      PrimitiveType(Number IntType);
      PrimitiveType(Number IntType);
    ]))));
    "ill-typed program: fail-func12"     >:: (ill_typed_test   "fail-func12"    (TypeCheckerErr(AccIdxToNotArr)));
    "ill-typed program: fail-func13"     >:: (ill_typed_test   "fail-func13"    (TypeCheckerErr(WrongActualParamsType("foo",
    [
      CompoundType(Pointer{pointed_type = Number IntType; indirection = 1});
      PrimitiveType(Number IntType)
    ],
    [
      CompoundType(Array{elements_type = Number IntType; indirection = 0; dimensions = 1; sizes = [(1, Some 10)]});
      PrimitiveType(Number IntType)
    ]))));
    "ill-typed program: fail-global1"     >:: (ill_symbol_test  "fail-global1"    (SymbolErr(VoidVarDecl("a"))));
    "ill-typed program: fail-global2"     >:: (ill_symbol_test  "fail-global2"    (SymbolErr(DuplicateEntry("b", Symbol.Var(PrimitiveType(Number IntType), false)))));
    "ill-typed program: fail-if1"         >:: (ill_typed_test   "fail-if1"        (TypeCheckerErr(GuardNotBool)));
    "ill-typed program: fail-local1"      >:: (ill_symbol_test  "fail-local1"     (SymbolErr(DuplicateEntry("i", Symbol.Var(PrimitiveType (Number IntType), false)))));
    "ill-typed program: fail-return1"     >:: (ill_typed_test   "fail-return1"    (TypeCheckerErr(WrongReturnType("main", PrimitiveType(Number IntType), PrimitiveType BoolType))));
    "ill-typed program: fail-return2"     >:: (ill_typed_test   "fail-return2"    (TypeCheckerErr(WrongReturnType("foo", PrimitiveType VoidType, PrimitiveType(Number IntType)))));
    "ill-typed program: fail-while1"      >:: (ill_typed_test   "fail-while1"     (TypeCheckerErr(GuardNotBool)));
    "ill-typed program: fail-string2"     >:: (ill_typed_test   "fail-string2"    (TypeCheckerErr(ArrNotAssignable(
      CompoundType(Array {
        elements_type = CharType;
        indirection = 0;
        dimensions = 1;
        sizes = [(1, Some 8)]
      })))));
    "ill-typed program: fail-ptr4"        >:: (ill_symbol_test  "fail-ptr4"   (SymbolErr(MultiDimArray "g")));
  ]

let _ = 
  run_test_tt_main failing_tests;
  run_test_tt_main successful_tests;