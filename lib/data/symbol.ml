(** Module intended to the definition of the various types of symbols *)

type is_extern = bool (** True if a global variable is external *)
and t = 
  | GlobalVar of Types.data_type * is_extern
  | LocalVar of Types.data_type * Location.code_pos
  | Fun of Types.primitive_type * (Types.data_type * Ast.identifier) list
[@@deriving show]