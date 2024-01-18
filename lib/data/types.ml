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
    elements_type   : primitive_type;
    indirection     : int;
    dimensions      : int;
    sizes           : (int * int option) list;
}
[@@deriving show]

type ptr_info = {
    pointed_type    : primitive_type;
    indirection     : int;
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
        pointed_type = primitive_type;
        indirection =  indirection;
      }
    | None -> failwith "Unexpected type conversion error"

let rec make_array_type typ dimensions sizes = match typ with
  | Ast.TypA(typ, size) -> 
    make_array_type typ (dimensions + 1) ((dimensions + 1, size)::sizes)
  | Ast.TypP typ -> 
    let ptr_info = get_ptr_info(typ, 1) in
    Array {
      elements_type = ptr_info.pointed_type;
      indirection = ptr_info.indirection;
      dimensions = dimensions;
      sizes = List.rev sizes;
    }
  | typ -> match convert_to_primitive_type typ with
    | Some primitive_type -> 
      Array {
        elements_type = primitive_type;
        indirection = 0;
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

let rec to_string t = match t with 
  | PrimitiveType VoidType -> "void"
  | PrimitiveType BoolType -> "bool"
  | PrimitiveType CharType -> "char"
  | PrimitiveType(Number IntType) -> "int"
  | PrimitiveType(Number FloatType) -> "float"
  | CompoundType(Array array_info) -> 
    (let size = snd(List.hd array_info.sizes) in 
    match size with 
    | Some size -> 
      String.concat "" [to_string (PrimitiveType (array_info.elements_type)); String.make array_info.indirection '*'; "["; Int.to_string size; "]"]
    | None -> 
      String.concat "" [to_string (PrimitiveType (array_info.elements_type)); "[]"])
  | CompoundType(Pointer ptr_info) -> 
    String.concat "" [to_string (PrimitiveType (ptr_info.pointed_type)); String.make ptr_info.indirection '*']