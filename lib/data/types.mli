(** Module intended to the definition of the data types of the language *)

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

(** [x] = compare [n1] [n2] 
    is greater than zero if [n1] is a float and [n2] is an int,
    less than zero if [n1] is an int and [n2] is a float, 
    and equal to zero if both [n1] and [n2] are int or float *)
val compare : number -> number -> int

(** [n] = max [n1] [n2] returns the maximum between [n1] and [n2]. Float is always larger than int *)
val max : number -> number -> number

(** Returns a string representation of the type [data_type] *)
val to_string : data_type -> string