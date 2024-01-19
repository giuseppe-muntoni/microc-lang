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

val compare : number -> number -> int

val max : number -> number -> number

val to_string : data_type -> string