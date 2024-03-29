open OUnit2
open Microc
open Test_utils

let expected_symbols_test_ex6 = 
  let open Types in 
  [
    [
      ("fac", Symbol.Fun(
        VoidType, 
        [
          (PrimitiveType(Number IntType), "n");
          (CompoundType(Pointer{pointed_type = Number(IntType); indirection = 1}), "res")
        ]
      ));
      ("getint", Symbol.Fun(Number IntType, []));
      ("main", Symbol.Fun(VoidType, []));
      ("print", Symbol.Fun(VoidType, [(PrimitiveType(Number IntType), "x")]));
      ("print_endline", Symbol.Fun(VoidType, [(CompoundType(Types.Array{elements_type = CharType; indirection = 0; dimensions = 1; sizes = [(1, None)]}), "str")]));
      ("print_float", Symbol.Fun(VoidType, [(PrimitiveType(Number FloatType), "x")]));
    ];
    [
      ("n", Symbol.LocalVar(PrimitiveType(Number IntType), Location.dummy_code_pos));
      ("res", Symbol.LocalVar(CompoundType(Pointer{pointed_type = Number(IntType); indirection = 1}), Location.dummy_code_pos))
    ];
    [
      ("tmp", Symbol.LocalVar(PrimitiveType(Number IntType), Location.dummy_code_pos))
    ];
    [
      ("i", Symbol.LocalVar(PrimitiveType(Number IntType), Location.dummy_code_pos));
      ("n", Symbol.LocalVar(PrimitiveType(Number IntType), Location.dummy_code_pos))
    ];
    [
      ("n", Symbol.LocalVar(PrimitiveType(Number IntType), Location.dummy_code_pos))
    ]
  ]

let to_string = fun all_symbols -> (
  String.concat "\n------------\n" (List.map (
    fun symbols -> 
      String.concat "\n" (List.map(fun (id, symbol) -> (String.concat ":" [id; Symbol.show symbol])) symbols);
  ) all_symbols
))

let create_test _ = 
  let program = Utils.create_ast "test-ex6" in 
  let res = Symbol_tables_repository.create program in 
  match res with 
  | Error(err) ->
    assert_bool (Semantic_errors.show (snd err)) false 
  | Ok() -> 
    let actual_symbols = List.map (Symbol_table.to_list) (Symbol_tables_repository.read_all ()) in
    assert_equal ~cmp: (fun l1 l2 -> List.equal (List.equal Utils.eq_symbol) l1 l2) ~printer: to_string expected_symbols_test_ex6 actual_symbols

let tests = "Tests for module Symbol_table_repository" >::: [
    "each symbol is inserted in the right scope" >:: create_test;
]

let _ = run_test_tt_main tests