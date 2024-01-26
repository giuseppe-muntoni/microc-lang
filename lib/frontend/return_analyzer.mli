(** Module intended to analyze the presence of the return in each function declaration *)

(** Given a [program] checks for each function declaration, if each possible execution path of the function has a return statement *)
val check_return_presence : Ast.program -> (unit, (Location.code_pos * Semantic_errors.t)) result