val create: Ast.program -> (unit, Location.code_pos * Semantic_errors.t) result
val read: Location.code_pos -> (Symbol.t Symbol_table.t, Location.code_pos * Semantic_errors.t) result
val read_all: unit -> (Symbol.t Symbol_table.t) list
val update: Location.code_pos -> (Symbol.t Symbol_table.t -> (Symbol.t Symbol_table.t, Semantic_errors.t) result) -> (unit, Location.code_pos * Semantic_errors.t) result
