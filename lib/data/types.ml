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