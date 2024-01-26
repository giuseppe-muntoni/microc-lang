(** This module is used to convert the types of the ast, which are recursive, to the types as defined in the module [Types] *)

(** [data_type] = adapt_ast_type [typ] produces the corresponding type of module [Types] of the ast type [typ] *)
val adapt_ast_type : Ast.typ -> Types.data_type