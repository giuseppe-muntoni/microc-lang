(** Type checker of the language *)

(** Given a program [program] checks that is well-typed *)
val check_types : Ast.program -> (unit, (Location.code_pos * Semantic_errors.t)) result