val build_fun : Ast.fun_decl -> Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Semantic_errors.t) result
val build_global_var : Ast.identifier -> Ast.typ -> Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Semantic_errors.t) result
val build_global_extern_var : Ast.identifier -> Ast.typ -> Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Semantic_errors.t) result
val build_local_var : Ast.identifier -> Ast.typ -> Location.code_pos -> Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Semantic_errors.t) result
val build_local_vars : (Ast.typ * Ast.identifier) list -> Location.code_pos -> Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Semantic_errors.t) result