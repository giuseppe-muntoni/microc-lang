type number = 
    | IntType 
    | FloatType
[@@deriving show]
  
type primitive_type = 
    | Number of number 
    | BoolType 
    | CharType 
    | VoidType
[@@deriving show]

type array_info = {
    primitive_type  : primitive_type;
    ptr_indirection : int;
    dimensions      : int;
    sizes           : (int * int option) list;
}
[@@deriving show]

type ptr_info = {
    primitive_type  : primitive_type;
    ptr_indirection : int;
}
[@@deriving show]

type compound_type = 
    | Pointer of ptr_info
    | Array of array_info
[@@deriving show]

type data_type = 
    | PrimitiveType of primitive_type 
    | CompoundType of compound_type
[@@deriving show]

let convert_to_primitive_type typ = match typ with 
  | Ast.TypI -> Some(Number IntType)
  | Ast.TypB -> Some(BoolType)
  | Ast.TypC -> Some(CharType)
  | Ast.TypF -> Some(Number FloatType)
  | Ast.TypV -> Some(VoidType)
  | _ -> None

let rec get_ptr_info (typ,indirection) = match typ with
  | Ast.TypP typ -> 
    get_ptr_info (typ,indirection+1)
  | Ast.TypA _ ->
    failwith "Unexpected type conversion error"
  | typ ->
    match convert_to_primitive_type typ with
    | Some primitive_type -> 
      { 
        primitive_type = primitive_type;
        ptr_indirection =  indirection;
      }
    | None -> failwith "Unexpected type conversion error"

let rec make_array_type typ dimensions sizes = match typ with
  | Ast.TypA(typ, size) -> 
    make_array_type typ (dimensions + 1) ((dimensions + 1, size)::sizes)
  | Ast.TypP typ -> 
    let ptr_info = get_ptr_info(typ, 1) in
    Array {
      primitive_type = ptr_info.primitive_type;
      ptr_indirection = ptr_info.ptr_indirection;
      dimensions = dimensions;
      sizes = List.rev sizes;
    }
  | typ -> match convert_to_primitive_type typ with
    | Some primitive_type -> 
      Array {
        primitive_type = primitive_type;
        ptr_indirection = 0;
        dimensions = dimensions;
        sizes = List.rev sizes;
      }
    | None -> failwith "Unexpected type conversion error"

let convert_to_data_type typ = match typ with
  | Ast.TypA(_, _) -> 
    let array_type = make_array_type typ 0 [] in
    CompoundType(array_type)
  | Ast.TypP typ -> 
    let ptr_info = get_ptr_info(typ, 1) in 
    CompoundType(Pointer(ptr_info))
  | _ -> 
    match convert_to_primitive_type typ with
    | Some primitive_type ->  
      PrimitiveType(primitive_type)
    | None -> 
      failwith "Unexpected type conversion error"

let compare a b = match a with
  | IntType -> ( match b with
    | IntType -> 0
    | FloatType -> -1
  )
  | FloatType -> ( match b with
    | FloatType -> 0
    | IntType -> 1
  )
  
let max a b = if compare a b >= 0 then a else b