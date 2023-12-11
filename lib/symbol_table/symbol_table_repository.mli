val create: Ast.program -> (unit, Location.code_pos * Errors.t) result
val read: Location.code_pos -> (Symbol.t Symbol_table.t, Location.code_pos * Errors.t) result
val update: Location.code_pos -> (Symbol.t Symbol_table.t -> Symbol.t Symbol_table.t) -> (unit, Location.code_pos * Errors.t) result
