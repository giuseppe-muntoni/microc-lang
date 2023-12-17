val build_fun : Ast.fun_decl -> Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Errors.t) result
val build_var : Ast.identifier -> Ast.typ -> Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Errors.t) result
val build_extern_var : Ast.identifier -> Ast.typ -> Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Errors.t) result
val build_vars : (Ast.typ * Ast.identifier) list -> Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Errors.t) result