let adapt_primitive_type_to_lltype typ llvm_context = 
  let open Types in
  match typ with
  | Number IntType -> 
    Llvm.i32_type llvm_context
  | Number FloatType -> 
    Llvm.float_type llvm_context
  | BoolType -> 
    Llvm.i1_type llvm_context
  | CharType -> 
    Llvm.i8_type llvm_context 
  | VoidType -> 
    Llvm.void_type llvm_context 

let rec adapt_pointer_to_lltype info llvm_context = 
  let open Types in 
  let _ = info.pointed_type (* Just to disambiguate *) in 
  match info.indirection with 
  | 1 -> Llvm.pointer_type (adapt_primitive_type_to_lltype info.pointed_type llvm_context)
  | n -> Llvm.pointer_type (adapt_pointer_to_lltype ({pointed_type = info.pointed_type; indirection = n - 1}) llvm_context)

let adapt_array_to_lltype info llvm_context = 
  let open Types in 
  let array_size = (
    match (List.hd info.sizes) with 
    | (_, Some n) -> n
    | (_, None) -> failwith "Unexpected error: size of the array not present!" 
  ) in 
  let elements_type = (
    match info.indirection with 
    | 0 -> 
      adapt_primitive_type_to_lltype info.elements_type llvm_context
    | n -> 
      let elements_type = {pointed_type = info.elements_type; indirection = n} in 
      adapt_pointer_to_lltype elements_type llvm_context
  ) in
  Llvm.array_type elements_type array_size 

let adapt_type_to_lltype typ llvm_context = 
  let open Types in
  match typ with
  | PrimitiveType(typ) -> 
    adapt_primitive_type_to_lltype typ llvm_context
  | CompoundType(Pointer ptr_info) -> 
    adapt_pointer_to_lltype ptr_info llvm_context
  | CompoundType(Array array_info) -> 
    adapt_array_to_lltype array_info llvm_context

let adapt_param_to_lltype typ llvm_context = 
  let open Types in
  match typ with
  | PrimitiveType(typ) -> 
    adapt_primitive_type_to_lltype typ llvm_context
  | CompoundType(Pointer ptr_info) -> 
    adapt_pointer_to_lltype ptr_info llvm_context
  | CompoundType(Array array_info) -> 
    let ptr_info = {
      pointed_type = array_info.elements_type;
      indirection = array_info.indirection + 1;
    } in 
    adapt_pointer_to_lltype ptr_info llvm_context

let adapt_symbol_to_lltype symbol llvm_context = 
  match symbol with 
  | Symbol.GlobalVar(typ, _)
  | Symbol.LocalVar(typ, _) ->
    adapt_type_to_lltype typ llvm_context
  | Symbol.Fun(ret_type, params) -> 
    let ret_type = adapt_primitive_type_to_lltype ret_type llvm_context in 
    let params_type = List.map (fun param -> 
      adapt_param_to_lltype (fst param) llvm_context
    ) params in 
    Llvm.function_type ret_type (Array.of_list params_type)
