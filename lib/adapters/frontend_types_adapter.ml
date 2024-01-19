open Types

let make_primitive_type typ = match typ with 
  | Ast.TypB -> BoolType
  | Ast.TypC -> CharType
  | Ast.TypI -> Number IntType
  | Ast.TypF -> Number FloatType
  | Ast.TypV -> VoidType
  | _ -> failwith "Unexpected type conversion error"

let get_ptr_info typ = 
  let rec get_ptr_info_aux typ indirection = match typ with
  | Ast.TypP typ -> 
    get_ptr_info_aux typ (indirection+1)
  | Ast.TypA _ ->
    failwith "Unexpected type conversion error"
  | typ -> { 
      pointed_type = make_primitive_type typ;
      indirection =  indirection;
    }
  in get_ptr_info_aux typ 1

let make_ptr_type typ = Pointer(get_ptr_info typ)

let make_array_type typ = 
  let rec make_array_type_aux typ dimensions sizes = match typ with
  | Ast.TypA(typ, size) -> 
    make_array_type_aux typ (dimensions + 1) ((dimensions + 1, size)::sizes)
  | Ast.TypP typ -> 
    let ptr_info = get_ptr_info typ in
    Array {
      elements_type = ptr_info.pointed_type;
      indirection = ptr_info.indirection;
      dimensions = dimensions;
      sizes = List.rev sizes;
    }
  | typ ->
    Array {
      elements_type = make_primitive_type typ;
      indirection = 0;
      dimensions = dimensions;
      sizes = List.rev sizes;
    }
  in make_array_type_aux typ 0 [] 

let adapt_ast_type typ = match typ with
  | Ast.TypA(_, _) -> 
    CompoundType(make_array_type typ)
  | Ast.TypP typ -> 
    CompoundType(make_ptr_type typ)
  | typ -> 
    PrimitiveType(make_primitive_type typ)
