(** Module intended to define the semantic analysis of the program *)

exception Semantic_errors of (Location.code_pos * string) list

(** Does the various semantic analyses for the [program] raising a [Semantic_errors] exception if there is some error *)
val check_semantic : Ast.program -> Ast.program
