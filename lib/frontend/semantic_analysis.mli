exception Semantic_errors of (Location.code_pos * string) list

val check_semantic : Ast.program -> Ast.program
