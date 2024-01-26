(** This module is intended to build the symbols and insert them into the symbol table *)

(** Builds a function symbol given the function declaration and inserts it into the specified symbol table *)
val build_fun : Ast.fun_decl -> Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Semantic_errors.t) result

(** Builds an internal global variable symbol given its identifier and type, and inserts it into the specified symbol table *)
val build_global_var : Ast.identifier -> Ast.typ -> Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Semantic_errors.t) result

(** Builds an external global variable symbol given its identifier and type, and inserts it into the specified symbol table *)
val build_global_extern_var : Ast.identifier -> Ast.typ -> Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Semantic_errors.t) result

(** Build a local variable symbol given its identifier, type and location of declaration, and inserts it into the specified symbol table *)
val build_local_var : Ast.identifier -> Ast.typ -> Location.code_pos -> Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Semantic_errors.t) result

(** Build a list of local variables symbols given their identifiers, types and a single location of declaration, and inserts them into the specified symbol table.
    Can be used for function parameters *)
val build_local_vars : (Ast.typ * Ast.identifier) list -> Location.code_pos -> Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Semantic_errors.t) result