(** This module is intended to detect deadcode in a program: deadcode can be unreachable statements or unused local variables or function params *)

(** Given a [program] returns a list of ([location], [deadcode_type]) pairs *)
val detect_deadcode : Ast.program -> (Location.code_pos * Semantic_errors.deadcode_type) list