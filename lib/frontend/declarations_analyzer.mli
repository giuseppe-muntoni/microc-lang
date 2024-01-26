(** This module is intended to checks the various declarations inside a program *)

(** Checks that an array specifies its size *)
val check_array_size: Ast.identifier -> Symbol.t -> Location.code_pos -> (unit, Location.code_pos * Semantic_errors.t) result

(** Checks that each variable accessed or function called is actually declared taking the [current_scope] of the expression [expr] and the [global_scope]*)
val check_accesses: Ast.expr -> Symbol.t Symbol_table.t -> Symbol.t Symbol_table.t -> (unit, Location.code_pos * Semantic_errors.t) result