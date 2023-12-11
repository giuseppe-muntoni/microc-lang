exception Semantic_error of Location.code_pos * string

val check_semantic : Ast.program -> Ast.program
