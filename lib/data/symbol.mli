type is_extern = bool
and t = 
  | Var of Types.data_type * is_extern
  | Fun of Types.primitive_type * Types.data_type list

val build_fun : Ast.fun_decl -> t Symbol_table.t -> t Symbol_table.t
val build_var : Ast.identifier -> Ast.typ -> t Symbol_table.t -> t Symbol_table.t
val build_extern_var : Ast.identifier -> Ast.typ -> t Symbol_table.t -> t Symbol_table.t
val build_vars : (Ast.typ * Ast.identifier) list -> t Symbol_table.t -> t Symbol_table.t