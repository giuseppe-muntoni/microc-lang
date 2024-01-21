type is_extern = bool
and t = 
  | GlobalVar of Types.data_type * is_extern
  | LocalVar of Types.data_type * Location.code_pos
  | Fun of Types.primitive_type * (Types.data_type * Ast.identifier) list
[@@deriving show]