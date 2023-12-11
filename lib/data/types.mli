type number = IntType | FloatType
  
type primitive_type = Number of number | BoolType | CharType | VoidType

type array_info = {
primitive_type  : primitive_type;
ptr_indirection : int;
dimensions      : int;
sizes           : (int * int option) list;
}

type ptr_info = {
primitive_type  : primitive_type;
ptr_indirection : int;
}

type compound_type = 
| Pointer of ptr_info
| Array of array_info

type data_type = PrimitiveType of primitive_type | CompoundType of compound_type

val convert_to_data_type : Ast.typ -> data_type

val compare : number -> number -> int

val max : number -> number -> number