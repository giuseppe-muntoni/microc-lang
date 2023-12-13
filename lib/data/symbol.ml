type is_extern = bool
and t = 
  | Var of Types.data_type * is_extern
  | Fun of Types.primitive_type * Types.data_type list