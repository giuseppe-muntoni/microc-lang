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

val convert_to_data_type : Ast.typ -> data_type

val compare : number -> number -> int

val max : number -> number -> number